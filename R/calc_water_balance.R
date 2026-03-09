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
#' @param level data frame of lake water level observations.. cols = Date,
#'  value
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
#' bind_rows
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_point geom_smooth theme_bw labs
#' @importFrom stats lm optim
#' @importFrom zoo rollmean
#'
#' @return data frame of water balance components which are:
#' - Date
#' - model
#' - value
#' - HYD_flow
#' - HYD_outflow
#' - area
#' - Ts
#' - T5avg
#' - evap_flux
#' - evap_m3
#' - rain
#' - deltaV
#' - ToT_inflow
#' - outflow
#'  
#'
#' @noRd
#'

calc_water_balance <- function(aeme_time, model, method, use, hyps, inf,
                               outf = NULL, level = NULL, init_elev, init_temp,
                               obs_lake = NULL, obs_met, elevation,
                               print_plots = FALSE, params = NULL,
                               coeffs = NULL) {
  
  # Set timezone temporarily to UTC
  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")
  model <- check_model(model = model)
  
  # Get dates to use for calculating the water balance
  max_spin <- max(unlist(aeme_time[["spin_up"]])[model])
  spin_start <- aeme_time[["start"]] - lubridate::ddays(max_spin + 6)
  date_stop <- aeme_time[["stop"]] + lubridate::ddays(1)
  date_vector <- seq.Date(from = as.Date(spin_start), to = as.Date(date_stop),
                          by = 1)
  surf <- elevation
  
  # If observations of level..
  if (use == "obs") {
    # if (is.null(level)) {
    #   stop("No observations of lake level provided")
    # }
    if (!is.null(level)) {
      cli_inform_safe(c("i" = "Using observed water level"))
      
      # Check observed water level is within hyps$elev range
      if (any(level$value < min(hyps$elev)) | any(level$value > max(hyps$elev))) {
        lvl_range <- range(level$value, na.rm = TRUE)
        elev_range <- range(hyps$elev, na.rm = TRUE)
        cli::cli_abort(c(
          "!" = "Observed water level values are outside the range of the hypsograph elevations.",
          "i" = "Observed water level range: {lvl_range[1]} to {lvl_range[2]}.",
          "i" = "Hypsograph elevation range: {elev_range[1]} to {elev_range[2]}."
        ))
      }
      
      
      # placeholder.. add optimised sin model here..!
      ampl <- ((quantile(level$value, 0.9) -
                  quantile(level$value, 0.1)) / 2) |>
        as.numeric()
      level <- level |>
        dplyr::select(Date, value) |>
        dplyr::rename(lvl_obs = value)
      offset <- 0
      mod_lvl <- data.frame(Date = obs_met$Date) |>
        dplyr::left_join(level, by = "Date", keep = FALSE) |>
        dplyr::mutate(
          is_obs_lvl = !is.na(lvl_obs)
        ) |> 
        dplyr::filter(Date >= spin_start & Date <= date_stop) 
      
      if (all(is.na(mod_lvl$lvl_obs))) {
        mod_lvl <- mod_lvl |> 
          dplyr::mutate(
            lvl_obs = rep(c(surf, rep(NA, 3)), length.out = dplyr::n()),
            is_obs_lvl = !is.na(lvl_obs)
          )
      }
      
      if (any(duplicated(mod_lvl$Date))) {
        warning(strwrap("Duplicate dates in observed water level data.\n
                        Only the first occurrence will be used."))
        mod_lvl <- mod_lvl |>
          dplyr::distinct(Date, .keep_all = TRUE)
      }
      
      if (all(!is.na(mod_lvl$lvl_obs))) {
        cli_inform_safe(c(i = "No missing values in observed water level.
                      Using observed water level"))
        
      } else {
        cli_inform_safe(c("!" ="Missing values in observed water level"))
      }
    } else {
      # Use constant water level
      cli_inform_safe(c(i = "No water level present.
                        Using constant water level."))
      ampl <- 0
      offset <- 0
      # Calculate the modelled water level
      mod_lvl <- data.frame(Date = obs_met$Date) |>
        dplyr::mutate(
          lvl_obs = rep(c(surf, rep(NA, 3)), length.out = dplyr::n()),
          is_obs_lvl = !is.na(lvl_obs)
        ) |> 
        dplyr::filter(Date >= spin_start & Date <= date_stop) 
      # mod_lvl <- data.frame(Date = date_vector)
      # mod_lvl <- mod_lvl |>
      #   dplyr::mutate(
      #     value = mod_lvl(Date, surf = surf,
      #                     ampl = ampl,
      #                     offset = offset)
      #   )
    }
  } else if (use == "mod") {
    mod_lvl <- level |>
      dplyr::filter(Date >= spin_start & Date <= date_stop)
    
    if (any(!mod_lvl$Date %in% date_vector)) {
      cli::cli_abort(c(
        "!" = "Modelled water level date range does not cover the simulation period.",
        "i" = "Expected range: {spin_start} to {date_stop}."
      ))
    }
  }
  
  # Prepare met data ----
  obs_met <- obs_met |>
    dplyr::mutate(MET_pprain = MET_pprain / 1000,
                  MET_ppsnow = MET_ppsnow / 1000, # convert to m
                  T5avg = zoo::rollmean(MET_tmpair, 5, na.pad = TRUE,
                                        align = c("right"))) 
  
  
  # If lake observations, use them to for evaporation estimations
  if (!is.null(obs_lake)) {
    sub <- obs_lake |>
      dplyr::filter(
        var_aeme == "HYD_temp",
        depth_from < 1,
        Date %in% obs_met$Date) |>
      dplyr::filter(!duplicated(Date)) |>
      dplyr::select(c("Date","value"))
    
    if (nrow(sub) == 0) {
      obs_lake <- NULL
      obs_met$HYD_temp <- NA
    } else {
      obs_met <- obs_met |>
        dplyr::left_join(sub, by = "Date") |>
        dplyr::rename(HYD_temp = value)
    }
  } else {
    if (!is.null(coeffs)) {
      cli_inform_safe(c("i" = "Using supplied coefficients for estimating lake
      surface temperature."))
      obs_met$HYD_temp <- coeffs[1] + coeffs[2] * obs_met$T5avg #
    }
  }
  
  # if less than 10 measurements
  if (sum(!is.na(obs_met[["HYD_temp"]])) < 10 & is.null(coeffs)) {
    cli_inform_safe(c("i" = "Insufficient lake temperature observations
                      to estimate surface temperature.
                      Using Stefan & Preud'homme (2007) method."))
    coeffs <- c(5, 0.75)
    obs_met$HYD_temp <- coeffs[1] + coeffs[2] * obs_met$T5avg # (Stefan & Preud'homme, 2007) www.doi.org/10.1111/j.1752-1688.1993.tb01502.x
  } else {
    fit <- lm(HYD_temp ~ T5avg, data = obs_met)
    coeffs <- coefficients(fit)
  }
  
  depth <- abs(min(hyps$depth))
  
  obs_met <- obs_met |> 
    estimate_surface_temperature(depth = depth)
  
  if (print_plots) {
    (ggplot2::ggplot(obs_met, ggplot2::aes(T5avg, sst)) +
       ggplot2::geom_point() +
       ggplot2::geom_smooth(span = 0.1, na.rm = TRUE, method = "lm") +
       ggplot2::theme_bw() + # theme(panel.border=element_blank(), axis.line=element_line()) +
       ggplot2::labs(x = NULL, y = NULL, colour = NULL) +
       ggplot2::theme(legend.position = 'none')) |>
      print()
  }
  
  if ("sst" %in% names(obs_met)) {
    col_select <- c("Date", "MET_wnduvu", "MET_wnduvv", "MET_tmpair",
                    "MET_humrel", "MET_prsttn", "MET_pprain", "sst")
  } else {
    col_select <- c("Date", "MET_wnduvu", "MET_wnduvv", "MET_tmpair",
                    "MET_humrel", "MET_prsttn", "MET_pprain")
  }
  
  gotm_met <- obs_met |>
    dplyr::select(all_of(col_select)) |>
    dplyr::rename(u10 = MET_wnduvu, v10 = MET_wnduvv, airt = MET_tmpair,
                  hum = MET_humrel, airp = MET_prsttn, precip = MET_pprain) |>
    dplyr::mutate(precip = precip / 86400, airp = airp) #|>

  
  # gotm_evap <- calc_evap(met = gotm_met, model ="gotm_wet")
  # glm_evap <- calc_evap(met = gotm_met, elevation = elevation,
  #                       model = "glm_aed")
  
  gotm_met <- gotm_met |>
    dplyr::filter(Date >= spin_start & Date <= date_stop) # filter dates
  dates <- seq.Date(gotm_met$Date[1], gotm_met$Date[nrow(gotm_met)], by = 1)
  length(dates) == nrow(gotm_met)
  
  # Set constants ----
  rho0 <- 1e3 # kg/m3
  Latent_Heat_Evap = 2.453E+6 # J/kg
  hyps <- hyps |> 
    dplyr::mutate(
      volume = calc_V(depth = elev, hyps = hyps)
    )
  
  # Calculate the fluctuating surface area
  wbal_init <- obs_met |>
    dplyr::left_join(mod_lvl, by = "Date", keep = FALSE) |>
    dplyr::filter(Date >= spin_start & Date <= date_stop)
  # nrow(wbal) == length(dates)
  if (any(duplicated(wbal_init$Date))) {
    # stop("Duplicated dates in the water balance data")
    cli::cli_abort(c(
      "!" = "Duplicate dates in the water balance data.",
      "i" = "Please check the input data for duplicates."
    ))
  }
  wbal <- lapply(model, \(m) {
    wbal_init |> 
      dplyr::mutate(
        model = m
      ) |> 
      dplyr::mutate(
        # area = get_hyps_val(depth = value, hyps = hyps),
        # Calculate 5-day average water temperature
        T5avg = zoo::rollmean(MET_tmpair, 5, na.pad = TRUE, align = c("right")),
        # apply the model to predict surface temperature
        Ts = sst,
        #saturation vapor pressure
        es = exp(2.3026 * (((7.5 * Ts) / (Ts + 237.3) + 0.7858))),
        #evaporative heat flux
        Qlh = (0.622/981.9) *         #constant/mean station pressure
          0.0013 *               #latent heat transfer coefficient
          1.168 *                #density of air
          2453000 *              #latent heat of evaporation of water
          MET_wndspd *           #wind speed in m/s
          (MET_prvapr - es),
        Qlh = dplyr::case_when(
          Qlh > 0 ~ 0,
          .default = Qlh
        ),
        #change in mass of surface layer
        # deltaM = ((-1 * Qlh) * area) / 2258000,
        #total evaporative loss
        # evap = deltaM * 86400 / 1000,
        # Evaporation rate
        # evap_flux = dplyr::case_when(
        #   model == "dy_cd" ~ -(evap / area) / 86400,
        #   model == "gotm_wet" ~ calc_evap(met = gotm_met, model = "gotm_wet",
        #                                   method = "fairall"),
        #   model == "glm_aed" ~ -(evap / area) / 86400,
        #   .default = 0
        # ),
        # evap_m3 = -evap_flux * area * 86400,
        # V = calc_V(depth = value, hyps = hyps, h = 0.01),
        # evap_rate2 = Qlh / Latent_Heat_Evap / rho0,
        # evap_rate3 = Qlh / Latent_Heat_Evap / wtr_density(Ts)
      )
  }) |> 
    dplyr::bind_rows()
  # apply the functions
  
  if (any(is.na(wbal$area))) {
    stop(strwrap("NA's in area. Most likely due to the hypsograph being too
                 small.\nConsider extending the elevation of the hypsograph with
                 the function `extrap_hyps(..., ext_elev = 5).` "))
  }
  
  # get total inflow discharge
  if (is.null(inf) | length(inf) == 0) {
    vol_inflow <- lapply(model, \(m) {
      data.frame(Date = obs_met$Date, HYD_flow = 0, model = m)
    }) |> 
      dplyr::bind_rows()
  } else {
    vol_inflow <- lapply(model, \(m) {
      df <- inf |>
        dplyr::bind_rows()
      if ((!"model" %in% names(df))) {
        df$model <- NA
      }
      df |> 
        dplyr::filter(model == m | is.na(model)) |>
        dplyr::select(c("Date", "HYD_flow")) |>
        dplyr::group_by(Date) |>
        dplyr::summarise(HYD_flow = sum(HYD_flow)) |> 
        dplyr::mutate(model = m)
    }) |> 
      dplyr::bind_rows()
  }
  
  # get total outflow discharge
  if (is.null(outf) | length(outf) == 0) {
    vol_outflow <- data.frame(Date = obs_met$Date, HYD_outflow = 0)
  } else {
    vol_outflow <- outf |>
      dplyr::bind_rows() |>
      dplyr::select(c("Date","outflow")) |>
      dplyr::group_by(Date) |>
      dplyr::summarise(HYD_outflow = sum(outflow))
  }
  
  # water balance ----
  wb_sub <- wbal
  obs_rain <- obs_met |> 
    dplyr::select(c("Date","MET_pprain"))
  
  wb <- lapply(model, \(m) {
    wb <- obs_met |>
      dplyr::select(Date) |>
      # add inflow discharge
      dplyr::left_join(vol_inflow, by = "Date") |>
      dplyr::filter(model == m) |> 
      dplyr::left_join(vol_outflow, by = "Date") |>
      # add evaporation estimation above
      dplyr::left_join(wb_sub, by = c("Date", "model")) |>
      dplyr::filter(Date >= spin_start & Date <= date_stop)
    
    # plot(wb)
    if (method %in% c(2, 3)) {
      wb <- wb |>
        estimate_lake_wlev(hyps_df = hyps, model = m, init_elev = init_elev, 
                           params = params)
    }
    
    return(wb)
  }) |> 
    dplyr::bind_rows()
  
  # Method 1 - No inflows or outflows
  if (method == 1) {
    wb <- wb |>
      dplyr::mutate(
        lvl_sim = NA,
        spill_outflow = 0,
        inflow = 0
      )
    # Method 2 - Outflows
  } else if (method == 2) {
    wb <- wb |>
      dplyr::mutate(
        inflow = HYD_flow
      )
    #   dplyr::group_by(model) |>
    #   # wb <- wb |> 
    #   estimate_lake_wlev(hyps_df = hyps, verbose = TRUE) |> 
    #   dplyr::ungroup()
    # Method 3 - Inflows and outflows
  } else  if (method == 3) {
    # Separate negative into inflows and positive into outflows
    wb <- wb |>
      dplyr::group_by(model) |> 
      dplyr::arrange(Date) |>
      dplyr::mutate(
        V = volume_from_level(h = lvl_sim, hyps = hyps),
        A_t = area_from_level(h = lvl_sim, hyps = hyps),
        dV = V - dplyr::lag(V),
        dV = dplyr::if_else(is.na(dV), 0, dV),
        expected_flux =
          HYD_flow +
          MET_pprain * A_t -
          evap_m3 -
          HYD_outflow -
          spill_outflow,
        residual = dV - expected_flux,
        eff_inflow = dplyr::if_else(residual > 0, residual, 0),
        eff_outflow = dplyr::if_else(residual < 0, -residual, 0),
        net_balance_m3 =
          HYD_flow + eff_inflow + MET_pprain * A_t -
          evap_m3 -
          HYD_outflow -
          spill_outflow - eff_outflow
      ) |> 
      dplyr::mutate(
        inflow = eff_inflow,
        spill_outflow = eff_outflow
      ) |>
      dplyr::select(-c(V, A_t, dV, expected_flux,
                       residual, eff_inflow, eff_outflow,
                       net_balance_m3)) |>
      dplyr::ungroup()
  }
  
  # Extract wbal parameters
  if (method %in% c(2, 3)) {
    wbal_params <- wb |> 
      # dplyr::group_by(model) |> 
      dplyr::summarise(
        C = mean(C), h_inv = mean(h_inv)
      )
  } else {
    wbal_params <- NULL
  } 
  
  
  if (print_plots) {
    wb |>
      tidyr::pivot_longer(cols = !contains(c("Date", "model")), names_to = "var",
                          values_to = "value") |>
      # gather(var,value,2:ncol(.)) |>
      ggplot2::ggplot(ggplot2::aes(x = Date, y = value, colour = model)) +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::geom_line() +
      ggplot2::ylab(bquote('Volume ('~m^-3~d^-1~')')) +
      ggplot2::xlab("Date") +
      ggplot2::theme_bw() +
      ggplot2::facet_wrap(~var, scales = 'free_y')
  }
  
  sel_cols <- c("Date", "model", "value",
                "inflow", "HYD_flow", "CHM_salt", "rain",
                "evap_m3", "evap_flux",
                "deltaV", "V",
                "Ts", "area",
                "HYD_outflow", "spill_outflow", "net")
  
  if ("lvl_sim" %in% names(wb)) {
    wb <- wb |> 
      dplyr::rename(value = lvl_sim)
  } else {
    wb <- wb |> 
      dplyr::mutate(value = init_elev)
  }
  
  wb_out <- wb |>
    dplyr::mutate(HYD_temp = Ts, CHM_salt = 0) |>
    # dplyr::rename(#outflow = spill_outflow,
    #               value = lvl_sim) |> 
    dplyr::mutate(
      area = area_from_level(h = value, hyps = hyps),
      V = volume_from_level(h = value, hyps = hyps),
      deltaV = c(0, diff(V)),
      rain = MET_pprain * area
      # net = HYD_flow + rain - HYD_outflow - outflow - evap_m3
    ) |> 
    dplyr::select(dplyr::any_of(sel_cols))
  
  # ANy missing cols set to NA
  missing_cols <- setdiff(sel_cols, names(wb_out))
  if (length(missing_cols) > 0) {
    for (col in missing_cols) {
      wb_out[[col]] <- NA
    }
  }
  
  out_list <- list(
    wb = wb_out,
    wbal_params = c("C" = wbal_params$C, "h_inv" = wbal_params$h_inv)
  )
  
  return(out_list)
}

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
  sapply(depth, function(l) approx(hyps[["elev"]], hyps[["area"]], xout = l,
                                   rule = 1)$y)
}

#' Calculate volume of a lake
#'
#' @param depth numeric; depth of the lake (m)
#' @inheritParams build_dycd
#' @param h numeric; depth intervals at which to calculate volume (m).
#' @noRd
calc_V <- function(depth, hyps, h = 0.1) {
  sapply(depth, \(d) {
    depths <- seq(min(hyps$elev), d, h)
    if (tail(depths, 1) != d) {
      depths <- c(depths, d)
    }
    areas <- approx(hyps[["elev"]], hyps[["area"]], depths)$y
    r <- sqrt((c(areas[-length(areas)]) / pi))
    R <- sqrt((areas[-1] / pi))
    V <- ((pi * h) / 3) * (R*R + R*r + r*r)
    # V <- numeric(length(areas) - 1)
    # for (i in 1:(length(areas) - 1)) {
    #   depth_diff <- depths[i + 1] - depths[i]
    #   area_avg <- (areas[i + 1] + areas[i]) / 2
    #   V[i] <- depth_diff * area_avg
    # }
    sum(V)
  })
}

#' Generate a sinusoidal water level for the lake
#' @param dates vector of dates
#' @param surf numeric; heightof the surface of the lake
#' @param ampl numeric; amplitude of the variation (m)
#' @param offset numeric; offset for the sinusoidal water level
#'
#' @importFrom lubridate year
#'
#' @noRd
mod_lvl <- function(dates, surf, ampl, offset) {
  daysinyear <- ifelse(as.numeric(strftime(dates, format = "%Y")) %% 4 == 0,
                       366, 365)
  DOY <- as.numeric(strftime(dates, format = "%j"))
  
  surf + ampl *(sin( ( (DOY * 2 * pi / daysinyear) + (offset))))
}

#' Calculate water density
#' @param wtr numeric vector of water temperature
#' @noRd
wtr_density <- function(wtr) {
  (1000 * (1 - (wtr + 288.9414) * (wtr - 3.9863)^2/(508929.2 *
                                                      (wtr + 68.12963))))
}

#' Optimise mod_lvl function
#' @param parameters numeric vector of two parameters to optimise; ampl and offset
#' @param mod_lvl data.frame; data.frame with Date and value columns
#' @noRd
optim_lvl_params <- function(parameters, mod_lvl, surf) {
  ampl <- parameters[1]
  offset <- parameters[2]
  
  # Call mod_lvl with the current ampl and offset values
  # Calculate the goodness of fit with your data
  predicted_values <- mod_lvl(mod_lvl$Date, surf = surf, ampl = ampl, offset = offset)
  residuals <- predicted_values - mod_lvl$value
  sum_of_squares <- sum(residuals^2, na.rm = TRUE)  # You can use a different error metric
  
  return(sum_of_squares)
}
