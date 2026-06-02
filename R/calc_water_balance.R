#' Estimate lake water balance based on a minimal set of inputs
#'
#' @param aeme_time list; time object from aeme_object using `time()`
#' @inheritParams build_aeme
#' @param method numeric; method to use for calculating water balance. Must be
#' 1 (no inflows or outflows) or 2 (outflows calculated) or 3 (inflows and
#' outflows calculated). Default = 1
#' @param use character; use observed or modelled lake level. Default = "obs".
#' @param hyps data frame of hypsographic curve, elevation (masl) and planar
#' area (m^2)
#' @param inf list of inflow data frames
#' @param outf list of outflow data frames. Default = NULL
#' @param level data frame of lake water level observations. cols = Date, value
#' @param obs_lake data frame of lake observations in ensemble standard format
#' @param obs_met data frame of meteorology, must include MET_tmpair, MET_wndspd
#'  & MET_prvapr, continuous Date, extent defines output extent
#' @param elevation numeric; elevation of lake
#' @param print_plots logical; print plots of water balance components
#' @param coeffs numeric vector; coefficients for estimating lake surface
#' temperature. Default = NULL
#'
#' @importFrom lubridate ddays
#' @importFrom withr local_locale local_timezone
#' @importFrom dplyr filter left_join mutate distinct group_by summarise
#' @importFrom dplyr bind_rows
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth theme_bw labs
#' @importFrom stats lm optim
#' @importFrom zoo rollmean
#' @importFrom cli cli_abort cli_inform cli_progress_step cli_progress_update
#' @importFrom cli cli_progress_done
#'
#' @return list with:
#' - `wb`: data frame of water balance components (Date, model, value,
#'   HYD_flow, HYD_outflow, area, Ts, T5avg, evap_flux, evap_m3, rain,
#'   deltaV, inflow, spill_outflow, net)
#' - `wbal_params`: named numeric vector of fitted parameters (C, h_inv),
#'   or NULL for method 1
#'
#' @noRd

calc_water_balance <- function(aeme_time, model, method, use, hyps, inf,
                               outf = NULL, level = NULL, init_elev, init_temp,
                               obs_lake = NULL, obs_met, elevation,
                               print_plots = FALSE, params = NULL,
                               coeffs = NULL) {
  
  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")
  cli_safe("Calculating water balance", FUN = cli::cli_h2)
  
  model <- check_model(model = model)
  
  # ---- Date range ----
  max_spin  <- max(unlist(aeme_time[["spin_up"]])[model])
  spin_start <- aeme_time[["start"]] - lubridate::ddays(max_spin + 1)
  date_stop  <- aeme_time[["stop"]]  + lubridate::ddays(1)
  surf       <- elevation
  
  # ---- Resolve water level ----
  mod_lvl <- resolve_water_level(
    use        = use,
    level      = level,
    obs_met    = obs_met,
    hyps       = hyps,
    surf       = surf,
    spin_start = spin_start,
    date_stop  = date_stop
  )
  
  # ---- Prepare met data ----
  obs_met <- obs_met |>
    dplyr::mutate(
      MET_pprain = MET_pprain / 1000,  # mm -> m
      MET_ppsnow = MET_ppsnow / 1000,
      T5avg = zoo::rollmean(MET_tmpair, 5, na.pad = TRUE, align = "right")
    )
  
  # ---- Estimate lake surface temperature ----
  obs_met <- obs_met |>
    add_surface_temperature(obs_lake = obs_lake, coeffs = coeffs) |>
    estimate_surface_temperature(depth = abs(min(hyps$depth)))
  
  if (print_plots) print_sst_plot(obs_met)
  
  # ---- Prepare GOTM met ----
  gotm_met <- prep_gotm_met(obs_met, spin_start, date_stop)
  
  # ---- Add hypsograph volumes ----
  hyps <- hyps |>
    dplyr::mutate(volume = calc_V(depth = elev, hyps = hyps))
  
  # ---- Build initial water balance frame ----
  wbal_init <- obs_met |>
    dplyr::left_join(mod_lvl, by = "Date", keep = FALSE) |>
    dplyr::filter(Date >= spin_start & Date <= date_stop)
  
  if (any(duplicated(wbal_init$Date))) {
    cli::cli_abort(c(
      "!" = "Duplicate dates in the water balance data.",
      "i" = "Please check the input data for duplicates."
    ))
  }
  
  # ---- Compute evaporation fluxes per model ----
  wbal <- lapply(model, \(m) {
    wbal_init |>
      dplyr::mutate(
        model = m,
        T5avg = zoo::rollmean(MET_tmpair, 5, fill = NA, align = "right"),
        Ts    = sst,
        es    = exp(2.3026 * ((7.5 * Ts) / (Ts + 237.3) + 0.7858)),
        Qlh   = (0.622 / 981.9) * 0.0013 * 1.168 * 2453000 *
          MET_wndspd * (MET_prvapr - es),
        Qlh   = dplyr::if_else(Qlh > 0, 0, Qlh)
      ) |> 
      tidyr::fill(T5avg, .direction = "up")
  }) |>
    dplyr::bind_rows()
  
  if (any(is.na(wbal$area))) {
    cli::cli_abort(c(
      "!" = "NAs in area - hypsograph may be too small.",
      "i" = "Consider extending elevation with `extrap_hyps(..., ext_elev = 5)`."
    ))
  }
  
  # ---- Aggregate inflows and outflows ----
  vol_inflow  <- aggregate_inflows(inf, model, obs_met)
  vol_outflow <- aggregate_outflows(outf, obs_met)
  
  obs_rain <- dplyr::select(obs_met, Date, MET_pprain)
  
  # ---- Assemble water balance per model ----
  wb <- lapply(model, \(m) {
    mod_inflow <- vol_inflow  |> 
      dplyr::filter(model == m) |> 
      dplyr::select(Date, HYD_flow) 
    wb_m <- obs_met |>
      dplyr::select(Date) |>
      dplyr::mutate(model = m) |> 
      dplyr::left_join(mod_inflow, by = "Date") |>
      dplyr::left_join(vol_outflow, by = "Date") |>
      dplyr::left_join(wbal        |> dplyr::filter(model == m),
                       by = c("Date", "model")) |>
      dplyr::filter(Date >= spin_start & Date <= date_stop)
    
    if (method %in% c(2, 3)) {
      wb_m <- wb_m |>
        estimate_lake_wlev(hyps_df = hyps, model = m, init_elev = init_elev,
                           params = params)
    }
    wb_m
  }) |>
    dplyr::bind_rows()
  
  # ---- Apply method-specific inflow/outflow logic ----
  wb <- apply_wb_method(wb, method, hyps)
  
  # ---- Extract fitted parameters ----
  wbal_params <- if (method %in% c(2, 3)) {
    dplyr::summarise(wb, C = mean(C), h_inv = mean(h_inv))
  } else {
    NULL
  }
  
  if (print_plots) print_wb_plot(wb)
  
  # ---- Final column selection and output ----
  sel_cols <- c("Date", "model", "value",
                "inflow", "HYD_flow", "CHM_salt", "rain",
                "evap_m3", "evap_flux",
                "deltaV", "V",
                "Ts", "area",
                "HYD_outflow", "spill_outflow", "net")
  
  wb <- if ("lvl_sim" %in% names(wb)) {
    dplyr::rename(wb, value = lvl_sim)
  } else {
    dplyr::mutate(wb, value = init_elev)
  }
  
  wb_out <- wb |>
    dplyr::mutate(
      HYD_temp = Ts,
      CHM_salt = 0,
      area     = area_from_level(h = value, hyps = hyps),
      V        = volume_from_level(h = value, hyps = hyps),
      deltaV   = c(0, diff(V)),
      rain     = MET_pprain * area
    ) |>
    dplyr::select(dplyr::any_of(sel_cols))
  
  # Fill any missing expected columns with NA
  missing_cols <- setdiff(sel_cols, names(wb_out))
  for (col in missing_cols) wb_out[[col]] <- NA
  
  list(
    wb          = wb_out,
    wbal_params = c("C" = wbal_params$C, "h_inv" = wbal_params$h_inv)
  )
}

#' Resolve observed or modelled water level into a common data frame
#' @noRd
#' @importFrom cli cli_abort cli_inform cli_progress_step cli_progress_update 
#' @importFrom cli cli_div cli_end
resolve_water_level <- function(use, level, obs_met, hyps, surf,
                                spin_start, date_stop) {
  
  FUN = cli::cli_inform
  cli_safe("Resolving water level", indent = FALSE)
  # on.exit({
  #   if (!is.null(pb_id)) cli::cli_progress_done(id = pb_id)
  # })  
  if (use == "mod") {
    date_vector <- seq.Date(as.Date(spin_start), as.Date(date_stop), by = 1)
    mod_lvl <- dplyr::filter(level, Date >= spin_start & Date <= date_stop)
    if (any(!mod_lvl$Date %in% date_vector)) {
      cli::cli_abort(c(
        "!" = "Modelled water level date range does not cover the simulation period.",
        "i" = "Expected range: {spin_start} to {date_stop}."
      ))
    }
    return(mod_lvl)
  }
  
  # use == "obs"
  if (!is.null(level)) {
    cli_safe(c("i" = "Using observed water level"), FUN = FUN)
    
    lvl_range  <- range(level$value, na.rm = TRUE)
    elev_range <- range(hyps$elev,   na.rm = TRUE)
    if (lvl_range[1] < elev_range[1] | lvl_range[2] > elev_range[2]) {
      cli::cli_abort(c(
        "!" = "Observed water level values are outside the hypsograph elevation range.",
        "i" = "Observed range: {lvl_range[1]} to {lvl_range[2]}.",
        "i" = "Hypsograph range: {elev_range[1]} to {elev_range[2]}."
      ))
    }
    
    mod_lvl <- data.frame(Date = obs_met$Date) |>
      dplyr::left_join(dplyr::select(dplyr::rename(level, lvl_obs = value),
                                     Date, lvl_obs),
                       by = "Date") |>
      dplyr::mutate(is_obs_lvl = !is.na(lvl_obs)) |>
      dplyr::filter(Date >= spin_start & Date <= date_stop)
    
    if (all(is.na(mod_lvl$lvl_obs))) {
      mod_lvl <- dplyr::mutate(
        mod_lvl,
        lvl_obs    = rep(c(surf, rep(NA, 3)), length.out = dplyr::n()),
        is_obs_lvl = !is.na(lvl_obs)
      )
    }
    
    if (any(duplicated(mod_lvl$Date))) {
      cli::cli_warn("Duplicate dates in observed water level - keeping first occurrence.")
      mod_lvl <- dplyr::distinct(mod_lvl, Date, .keep_all = TRUE)
    }
    
    if (all(!is.na(mod_lvl$lvl_obs))) {
      cli_safe(c("v" = "No missing values in observed water level"),
               FUN = FUN)
    } else {
      cli_safe("Missing values in observed water level", FUN = cli::cli_alert_warning)
    }
    
  } else {
    cli_safe(c("i" = "No water level present. Using constant water level."), 
             FUN = FUN)
    mod_lvl <- data.frame(Date = obs_met$Date) |>
      dplyr::mutate(
        lvl_obs    = rep(c(surf, rep(NA, 3)), length.out = dplyr::n()),
        is_obs_lvl = !is.na(lvl_obs)
      ) |>
      dplyr::filter(Date >= spin_start & Date <= date_stop)
  }
  
  mod_lvl
}


#' Add HYD_temp column to obs_met from lake obs or coefficients
#' @noRd
add_surface_temperature <- function(obs_met, obs_lake, coeffs) {
  if (!is.null(obs_lake)) {
    sub <- obs_lake |>
      dplyr::filter(var_aeme == "HYD_temp", depth_from < 1,
                    Date %in% obs_met$Date) |>
      dplyr::filter(!duplicated(Date)) |>
      dplyr::select(Date, value)
    
    if (nrow(sub) == 0) {
      obs_met$HYD_temp <- NA
    } else {
      obs_met <- obs_met |>
        dplyr::left_join(sub, by = "Date") |>
        dplyr::rename(HYD_temp = value)
    }
  } else if (!is.null(coeffs)) {
    cli_inform_safe(c("i" = "Using supplied coefficients for estimating lake 
                      surface temperature."))
    obs_met$HYD_temp <- coeffs[1] + coeffs[2] * obs_met$T5avg
  } else {
    obs_met$HYD_temp <- NA
  }
  
  # Fall back to Stefan & Preud'homme if fewer than 10 valid observations
  n_obs <- sum(!is.na(obs_met$HYD_temp))
  if (n_obs < 10) {
    cli_inform_safe(c(
      "i" = "Insufficient lake temperature observations (<10).",
      "i" = "Using Stefan & Preud'homme (2007) method to estimate surface 
      temperature."
    ))
    coeffs <- c(5, 0.75)
    obs_met$HYD_temp <- coeffs[1] + coeffs[2] * obs_met$T5avg
  } else {
    fit    <- lm(HYD_temp ~ T5avg, data = obs_met)
    coeffs <- stats::coefficients(fit)  # nolint - used for side-effect doc only
  }
  
  obs_met
}


#' Prepare GOTM-formatted met data frame
#' @noRd
prep_gotm_met <- function(obs_met, spin_start, date_stop) {
  col_select <- c("Date", "MET_wnduvu", "MET_wnduvv", "MET_tmpair",
                  "MET_humrel", "MET_prsttn", "MET_pprain",
                  if ("sst" %in% names(obs_met)) "sst")
  
  obs_met |>
    dplyr::select(dplyr::all_of(col_select)) |>
    dplyr::rename(u10 = MET_wnduvu, v10 = MET_wnduvv, airt = MET_tmpair,
                  hum  = MET_humrel, airp = MET_prsttn, precip = MET_pprain) |>
    dplyr::mutate(precip = precip / 86400) |>
    dplyr::filter(Date >= spin_start & Date <= date_stop)
}


#' Aggregate inflows across tributaries per model
#' @noRd
aggregate_inflows <- function(inf, model, obs_met) {
  if (is.null(inf) || length(inf) == 0) {
    lapply(model, \(m) data.frame(Date = obs_met$Date, HYD_flow = 0, model = m)) |>
      dplyr::bind_rows()
  } else {
    lapply(model, \(m) {
      df <- dplyr::bind_rows(inf)
      if (!"model" %in% names(df)) df$model <- NA
      df |>
        dplyr::filter(model == m | is.na(model)) |>
        dplyr::select(Date, HYD_flow) |>
        dplyr::group_by(Date) |>
        dplyr::summarise(HYD_flow = sum(HYD_flow), .groups = "drop") |>
        dplyr::mutate(model = m)
    }) |>
      dplyr::bind_rows()
  }
}


#' Aggregate outflows across outlets
#' @noRd
aggregate_outflows <- function(outf, obs_met) {
  if (is.null(outf) || length(outf) == 0) {
    data.frame(Date = obs_met$Date, HYD_outflow = 0)
  } else {
    dplyr::bind_rows(outf) |>
      dplyr::select(Date, HYD_flow) |>
      dplyr::group_by(Date) |>
      dplyr::summarise(HYD_outflow = sum(HYD_flow), .groups = "drop")
  }
}


#' Apply water balance method to wb data frame
#' @noRd
apply_wb_method <- function(wb, method, hyps) {
  if (method == 1) {
    lake_surf <- hyps |> 
      dplyr::filter(depth == 0) 
    wb |>
      dplyr::rename(
        inflow = HYD_flow
      ) |> 
      dplyr::mutate(
        lvl_sim = lake_surf$elev,
        spill_outflow = 0,
        area = area_from_level(h = lvl_sim, hyps = hyps),
        Qlh_t = latent_heat_flux(Ts = Ts, wndspd = MET_wndspd, 
                                 prvapr = MET_prvapr,
                                 prsttn = (MET_prsttn / 100)),
        evap_flux = flux_to_evap(Qlh = Qlh_t),
        evap_m3 = evap_flux * area,
        rain = MET_pprain * area,
        net = (inflow + rain - evap_m3 - HYD_outflow - spill_outflow)
      )
  } else if (method == 2) {
    dplyr::mutate(wb, inflow = HYD_flow)
    
  } else if (method == 3) {
    wb |>
      dplyr::group_by(model) |>
      dplyr::arrange(Date) |>
      dplyr::mutate(
        V            = volume_from_level(h = lvl_sim, hyps = hyps),
        A_t          = area_from_level(h = lvl_sim, hyps = hyps),
        dV           = dplyr::coalesce(V - dplyr::lag(V), 0),
        expected_flux = HYD_flow + MET_pprain * A_t - evap_m3 -
          HYD_outflow - spill_outflow,
        residual     = dV - expected_flux,
        inflow       = dplyr::if_else(residual > 0,  residual,  0),
        spill_outflow = dplyr::if_else(residual < 0, -residual, 0)
      ) |>
      dplyr::select(-V, -A_t, -dV, -expected_flux, -residual) |>
      dplyr::ungroup()
  }
}


#' Print SST vs T5avg diagnostic plot
#' @noRd
print_sst_plot <- function(obs_met) {
  p <- ggplot2::ggplot(obs_met, ggplot2::aes(T5avg, sst)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(span = 0.1, na.rm = TRUE, method = "lm") +
    ggplot2::theme_bw() +
    ggplot2::labs(x = NULL, y = NULL, colour = NULL) +
    ggplot2::theme(legend.position = "none")
  print(p)
}


#' Print water balance time series plot
#' @noRd
print_wb_plot <- function(wb) {
  p <- wb |>
    tidyr::pivot_longer(
      cols      = !dplyr::contains(c("Date", "model")),
      names_to  = "var",
      values_to = "value"
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = Date, y = value, colour = model)) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_line() +
    ggplot2::ylab(bquote("Volume (" ~ m^-3 ~ d^-1 ~ ")")) +
    ggplot2::xlab("Date") +
    ggplot2::theme_bw() +
    ggplot2::facet_wrap(~var, scales = "free_y")
  print(p)
}


# ---- Utility functions ----

level_from_volume <- function(V, hyps) {
  approx(hyps$volume, hyps$elev, V, rule = 2)$y
}

volume_from_level <- function(h, hyps) {
  approx(hyps$elev, hyps$volume, h, rule = 2)$y
}

area_from_level <- function(h, hyps) {
  approx(hyps$elev, hyps$area, h, rule = 2)$y
}

#' Calculate actual surface area at a specific depth
#' @param depth numeric; depth of the lake (m)
#' @inheritParams build_dycd
#' @noRd
get_hyps_val <- function(depth, hyps) {
  sapply(depth, \(l) approx(hyps[["elev"]], hyps[["area"]], xout = l, rule = 1)$y)
}

#' Calculate volume of a lake using frustum method
#' @param depth numeric; depth of the lake (m)
#' @inheritParams build_dycd
#' @param h numeric; depth intervals at which to calculate volume (m)
#' @noRd
calc_V <- function(depth, hyps, h = 0.1) {
  sapply(depth, \(d) {
    depths <- seq(min(hyps$elev), d, h)
    if (tail(depths, 1) != d) depths <- c(depths, d)
    areas <- approx(hyps[["elev"]], hyps[["area"]], depths)$y
    r <- sqrt(areas[-length(areas)] / pi)
    R <- sqrt(areas[-1] / pi)
    sum((pi * h / 3) * (R^2 + R * r + r^2))
  })
}

#' Generate a sinusoidal water level time series
#' @param dates vector of dates
#' @param surf numeric; height of the lake surface (m)
#' @param ampl numeric; amplitude of variation (m)
#' @param offset numeric; phase offset for the sinusoidal curve
#' @importFrom lubridate year
#' @noRd
mod_lvl <- function(dates, surf, ampl, offset) {
  days_in_year <- ifelse(as.numeric(strftime(dates, "%Y")) %% 4 == 0, 366, 365)
  doy <- as.numeric(strftime(dates, "%j"))
  surf + ampl * sin(doy * 2 * pi / days_in_year + offset)
}

#' Calculate water density from temperature
#' @param wtr numeric vector of water temperature (degC)
#' @noRd
wtr_density <- function(wtr) {
  1000 * (1 - (wtr + 288.9414) * (wtr - 3.9863)^2 / (508929.2 * (wtr + 68.12963)))
}

#' Optimise sinusoidal water level parameters
#' @param parameters numeric vector of length 2; (ampl, offset)
#' @param mod_lvl data.frame; with Date and value columns
#' @param surf numeric; surface elevation
#' @noRd
optim_lvl_params <- function(parameters, mod_lvl, surf) {
  predicted <- mod_lvl(mod_lvl$Date, surf = surf,
                       ampl   = parameters[1],
                       offset = parameters[2])
  sum((predicted - mod_lvl$value)^2, na.rm = TRUE)
}

#' Saturation vapour pressure
#'
#' Calculates saturation vapour pressure at the water surface using the
#' Magnus formula.
#'
#' @param Ts Numeric. Water surface temperature (°C).
#'
#' @return Numeric. Saturation vapour pressure (hPa).
#'
#' @examples
#' sat_vapour_pressure(20)
#' sat_vapour_pressure(c(15, 20, 25))
sat_vapour_pressure <- function(Ts) {
  exp(2.3026 * ((7.5 * Ts) / (Ts + 237.3) + 0.7858))
}


#' Latent heat flux
#'
#' Calculates latent heat flux from a lake surface using the bulk aerodynamic
#' method. Flux is capped at zero — only heat loss from the water is retained.
#'
#' @param Ts      Numeric. Water surface temperature (°C).
#' @param wndspd  Numeric. Wind speed (m/s).
#' @param prvapr  Numeric. Air vapour pressure (hPa).
#' @param P       Numeric. Atmospheric pressure (hPa). Default 981.9.
#' @param Ce      Numeric. Bulk transfer coefficient (Dalton number). Default 0.0013.
#' @param rho_air Numeric. Air density (kg/m³). Default 1.168.
#' @param Lv      Numeric. Latent heat of vaporisation (J/kg). Default 2453000.
#'
#' @return Numeric. Latent heat flux (W/m²), <= 0.
#'
#' @seealso [sat_vapour_pressure()], [flux_to_evap()]
#'
#' @examples
#' latent_heat_flux(Ts = 20, wndspd = 3, prvapr = 10)
#'
#' # Vectorised over a data frame
#' latent_heat_flux(Ts     = data$sst,
#'                  wndspd = data$MET_wndspd,
#'                  prvapr = data$MET_prvapr)
latent_heat_flux <- function(Ts, wndspd, prvapr,
                             prsttn = 981.9, Ce = 0.0013,
                             rho_air = 1.168, Lv = 2453000) {
  es  <- sat_vapour_pressure(Ts)
  Qlh <- (0.622 / prsttn) * Ce * rho_air * Lv * wndspd * (prvapr - es)
  pmin(Qlh, 0)
}


#' Convert latent heat flux to evaporation depth
#'
#' Converts latent heat flux (W/m²) to an evaporation rate in metres per day,
#' suitable for lake water balance calculations.
#'
#' @param Qlh       Numeric. Latent heat flux (W/m²), should be <= 0.
#' @param Lv        Numeric. Latent heat of vaporisation (J/kg). Default 2453000.
#' @param rho_water Numeric. Water density (kg/m³). Default 1000.
#'
#' @return Numeric. Evaporation rate (m/day), <= 0.
#'
#' @seealso [latent_heat_flux()]
#'
#' @examples
#' flux_to_evap(-50)
#'
#' # Full pipeline
#' Qlh  <- latent_heat_flux(Ts = data$sst, wndspd = data$MET_wndspd, prvapr = data$MET_prvapr)
#' evap <- flux_to_evap(Qlh)
flux_to_evap <- function(Qlh, Lv = 2453000, rho_water = 1000) {
  (Qlh / Lv) * (86400 / rho_water)
}

