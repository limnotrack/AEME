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
                               outf = NULL, level = NULL, obs_lake = NULL,
                               obs_met, elevation,
                               print_plots = FALSE, coeffs = NULL,
                               print = TRUE) {
  
  # Set timezone temporarily to UTC
  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")
  
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
      if (print) {
        message("Using observed water level")
      }
      # placeholder.. add optimised sin model here..!
      ampl <- ((quantile(level$value, 0.9) -
                  quantile(level$value, 0.1)) / 2) |>
        as.numeric()
      level <- level |>
        dplyr::select(Date, value)
      offset <- 0
      mod.lvl <- data.frame(Date = obs_met$Date) |>
        dplyr::left_join(level, by = "Date", keep = FALSE) |>
        dplyr::filter(Date >= spin_start & Date <= date_stop) |>
        dplyr::mutate(var_aeme = "LKE_lvlwtr")
      
      if (any(duplicated(mod.lvl$Date))) {
        warning(strwrap("Duplicate dates in observed water level data.\n
                        Only the first occurrence will be used."))
        mod.lvl <- mod.lvl |>
          dplyr::distinct(Date, .keep_all = TRUE)
      }
      
      if (all(!is.na(mod.lvl$value))) {
        if (print) {
          message(strwrap("No missing values in observed water level.
                      Using observed water level"))
        }
      } else {
        if (print) {
          message("Missing values in observed water level")
        }
        # Number of observations
        n_lvl <- sum(!is.na(mod.lvl$value))
        
        # If there are greater than or equal to 9 observations, use the
        # optimisation function
        if (n_lvl >= 9) {
          if (print) {
            message("Using optimisation function")
          }
          # Initial parameter values
          initial_parameters <- c(ampl = ampl, offset = offset)
          # optim_lvl_params(initial_parameters, mod.lvl = mod.lvl, surf = surf)
          
          # Optimize the parameters
          optimized_parameters <- stats::optim(par = initial_parameters,
                                               fn = optim_lvl_params,
                                               mod.lvl = mod.lvl, surf = surf,
                                               method = "L-BFGS-B")
          ampl <- optimized_parameters$par["ampl"]
          offset <- optimized_parameters$par["offset"]
        } else {
          # Use constant water level
          if (print) {
            message("Using constant water level")
          }
          ampl <- 0
          offset <- 0
        }
        
        # Calculate the modelled water level
        mod.lvl <- mod.lvl |>
          dplyr::mutate(
            value = mod_lvl(Date, surf = surf,
                            ampl = ampl,
                            offset = offset)
          )
      }
    } else {
      # Use constant water level
      if (print) {
        message("No water level present. Using constant water level.")
      }
      ampl <- 0
      offset <- 0
      # Calculate the modelled water level
      mod.lvl <- data.frame(Date = date_vector)
      mod.lvl <- mod.lvl |>
        dplyr::mutate(
          value = mod_lvl(Date, surf = surf,
                          ampl = ampl,
                          offset = offset)
        )
    }
  } else if (use == "mod") {
    mod.lvl <- level |>
      dplyr::filter(Date >= spin_start & Date <= date_stop)
    
    if (any(!mod.lvl$Date %in% date_vector)) {
      stop(strwrap(paste0("Modelled water level date range does not cover the
                          simulation period (", spin_start, " to ", date_stop,
                          "). ")))
    }
  }
  
  # Prepare met data ----
  obs_met <- obs_met |>
    dplyr::mutate(MET_pprain = MET_pprain / 1000,
                  MET_ppsnow = MET_ppsnow / 1000) |> # convert to m
    dplyr::mutate(T5avg = zoo::rollmean(MET_tmpair, 5, na.pad = TRUE,
                                        align = c("right")))
  
  # Evaporation ----
  evap <- obs_met |>
    dplyr::select(c("Date","MET_tmpair", "MET_prvapr","MET_wndspd")) |>
    dplyr::mutate(T5avg = zoo::rollmean(MET_tmpair, 5, na.pad = TRUE,
                                        align = c("right")))
  
  # If lake observations, use them to for evaporation estimations
  if (!is.null(obs_lake)) {
    sub <- obs_lake |>
      dplyr::filter(
        var_aeme == "HYD_temp",
        depth_from < 1,
        Date %in% evap$Date) |>
      dplyr::filter(!duplicated(Date)) |>
      dplyr::select(c("Date","value"))
    
    if (nrow(sub) == 0) {
      obs_lake <- NULL
    } else {
      evap <- evap |>
        dplyr::left_join(sub, by = "Date")
    }
  } else {
    if (!is.null(coeffs)) {
      if (print) {
        message("Estimating temperature using supplied coefficients...")
      }
      evap$value <- coeffs[1] + coeffs[2] * evap$T5avg #
    }
  }
  
  # if less than 10 measurements
  if (sum(!is.na(evap[["value"]])) < 10 & is.null(coeffs)) {
    if (print) {
      message("Estimating temperature using Stefan & Preud'homme (2007)...")
    }
    coeffs <- c(5, 0.75)
    evap$value <- coeffs[1] + coeffs[2] * evap$T5avg # (Stefan & Preud'homme, 2007) www.doi.org/10.1111/j.1752-1688.1993.tb01502.x
  } else {
    fit <- stats::lm(value ~ T5avg, data = evap)
    coeffs <- coefficients(fit)
  }
  
  if (print_plots) {
    (ggplot2::ggplot(evap, ggplot2::aes(T5avg, value)) +
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
  
  # Estimate lake surface temperature (use sea surface temperature abbrev 'sst')
  if (!("sst" %in% names(obs_met))) {
    gotm_met <- gotm_met |>
      dplyr::mutate(sst = airt * coeffs[2] + coeffs[1])
    obs_met <- obs_met |>
      dplyr::mutate(sst = T5avg * coeffs[2] + coeffs[1])
  }
  
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
  
  # Calculate the fluctuating surface area
  wbal_init <- obs_met |>
    dplyr::left_join(mod.lvl, by = "Date", keep = FALSE) |>
    dplyr::filter(Date >= spin_start & Date <= date_stop)
  # nrow(wbal) == length(dates)
  if (any(duplicated(wbal_init$Date))) {
    stop("Duplicated dates in the water balance data")
  }
  wbal <- lapply(model, \(m) {
    wbal_init |> 
      dplyr::mutate(
        model = m
      ) |> 
      dplyr::mutate(
        area = get_hyps_val(depth = value, hyps = hyps),
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
        deltaM = ((-1 * Qlh) * area) / 2258000,
        #total evaporative loss
        evap = deltaM * 86400 / 1000,
        # Evaporation rate
        evap_flux = dplyr::case_when(
          model == "dy_cd" ~ -(evap / area) / 86400,
          model == "gotm_wet" ~ calc_evap(met = gotm_met, model = "gotm_wet",
                                          method = "fairall"),
          model == "glm_aed" ~ -(evap / area) / 86400,
          .default = 0
        ),
        evap_m3 = -evap_flux * area * 86400,
        # dy_cd_evap_flux = -(evap / area) / 86400,
        # gotm_wet_evap_flux = calc_evap(met = gotm_met, model = "gotm_wet",
        #                                method = "fairall"),
        # glm_aed_evap_flux = dy_cd_evap_flux,
        # glm_aed_evap_flux = calc_evap(met = gotm_met, elevation = elevation,
        #                               model = "glm_aed"),
        # dy_cd_evap_m3 = -dy_cd_evap_flux * area * 86400,
        # gotm_wet_evap_m3 = -gotm_wet_evap_flux * area * 86400,
        # glm_aed_evap_m3 = -glm_aed_evap_flux * area * 86400,
        V = calc_V(depth = value, hyps = hyps, h = 0.01),
        evap_rate2 = Qlh / Latent_Heat_Evap / rho0,
        evap_rate3 = Qlh / Latent_Heat_Evap / wtr_density(Ts)
      )
  }) |> 
    dplyr::bind_rows()
  
  # wbal <- wbal |> # filter dates
  #   # calculate the fluctuating surface area
  #   # dplyr::mutate(lvlwtr2 = mod_lvl(Date, surf = max(hyps[,1]), ampl = ampl,
  #   #                                 offset = offset)) |>
  #   dplyr::mutate(
  #     area = get_hyps_val(depth = value, hyps = hyps),
  #     # Calculate 5-day average water temperature
  #     T5avg = zoo::rollmean(MET_tmpair, 5, na.pad = TRUE, align = c("right")),
  #     # apply the model to predict surface temperature
  #     Ts = sst,
  #     #saturation vapor pressure
  #     es = exp(2.3026 * (((7.5 * Ts) / (Ts + 237.3) + 0.7858))),
  #     #evaporative heat flux
  #     Qlh = (0.622/981.9) *         #constant/mean station pressure
  #       0.0013 *               #latent heat transfer coefficient
  #       1.168 *                #density of air
  #       2453000 *              #latent heat of evaporation of water
  #       MET_wndspd *           #wind speed in m/s
  #       (MET_prvapr - es),
  #     #change in mass of surface layer
  #     deltaM = ((-1 * Qlh) * area) / 2258000,
  #     #total evaporative loss
  #     evap = deltaM * 86400 / 1000,
  #     # Evaporation rate
  #     dy_cd_evap_flux = -(evap / area) / 86400,
  #     gotm_wet_evap_flux = calc_evap(met = gotm_met, model = "gotm_wet",
  #                                    method = "fairall"),
  #     glm_aed_evap_flux = dy_cd_evap_flux,
  #     # glm_aed_evap_flux = calc_evap(met = gotm_met, elevation = elevation,
  #     #                               model = "glm_aed"),
  #     dy_cd_evap_m3 = -dy_cd_evap_flux * area * 86400,
  #     gotm_wet_evap_m3 = -gotm_wet_evap_flux * area * 86400,
  #     glm_aed_evap_m3 = -glm_aed_evap_flux * area * 86400,
  #     V = calc_V(depth = value, hyps = hyps, h = 0.01),
  #     evap_rate2 = Qlh / Latent_Heat_Evap / rho0,
  #     evap_rate3 = Qlh / Latent_Heat_Evap / wtr_density(Ts)
  #   ) |>
  #   dplyr::mutate(Qlh = dplyr::case_when(
  #     Qlh > 0 ~ 0,
  #     .default = Qlh
  #   ))
  # apply the functions
  
  # V = calc_V(depth = evap$value, hyps = hyps)
  
  if (any(is.na(wbal$area))) {
    stop(strwrap("NA's in area. Most likely due to the hypsograph being too
                 small.\nConsider extending the elevation of the hypsograph with
                 the function `extrap_hyps(..., ext_elev = 5).` "))
  }
  
  # get total inflow discharge
  if (is.null(inf) | length(inf) == 0) {
    vol.inflow <- lapply(model, \(m) {
      data.frame(Date = obs_met$Date, HYD_flow = 0, model = m)
    }) |> 
      dplyr::bind_rows()
  } else {
    vol.inflow <- lapply(model, \(m) {
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
    vol.outflow <- data.frame(Date = obs_met$Date, HYD_outflow = 0)
  } else {
    vol.outflow <- outf |>
      dplyr::bind_rows() |>
      dplyr::select(c("Date","outflow")) |>
      dplyr::group_by(Date) |>
      dplyr::summarise(HYD_outflow = sum(outflow))
  }
  
  # water balance ----
  wb_sub <- wbal |> 
    dplyr::select(c("Date", "model", "evap_m3", "evap_flux",
                    "T5avg", "Ts", "area", "value", "V"))
  obs_rain <- obs_met |> 
    dplyr::select(c("Date","MET_pprain"))
  
  wb <- lapply(model, \(m) {
    wb <- obs_met |>
      dplyr::select(Date) |>
      
      # add inflow discharge from fwmt model
      dplyr::left_join(vol.inflow, by = "Date") |>
      dplyr::filter(model == m) |> 
      dplyr::left_join(vol.outflow, by = "Date") |>
      
      # add evaporation estimation above
      dplyr::left_join(wb_sub, by = c("Date", "model")) |>
      dplyr::left_join(obs_rain, by = "Date") |> 
      dplyr::filter(Date >= spin_start & Date <= date_stop) |>
      # rainfall direct to surface of lake
      dplyr::mutate(rain = MET_pprain * area,
                    # calculate outflow by difference
                    deltaV = c(0, diff(value)) * area,
                    ToT_inflow = HYD_flow + rain,
                    outflow = ((HYD_flow + rain) -
                                 (HYD_outflow + evap_m3 + deltaV))
                    # outflow_dy_cd = ((HYD_flow + rain) -
                    #                    (HYD_outflow + dy_cd_evap_m3 + deltaV)),
                    # outflow_glm_aed = ((HYD_flow + rain) -
                    #                      (HYD_outflow + glm_aed_evap_m3 + deltaV)),
                    # outflow_gotm_wet = ((HYD_flow + rain) -
                    #                       (HYD_outflow + gotm_wet_evap_m3 + deltaV)),
                    # sum_gotm_wet = HYD_flow + rain
      )
    # wb[wb$Date == "2020-07-24", ]
    
    # wb <- wb[complete.cases(wb$outflow), ]
    # wb <- wb[complete.cases(wb$outflow_dy_cd), ]
    # wb <- wb[complete.cases(wb$outflow_glm_aed), ]
    # wb <- wb[complete.cases(wb$outflow_gotm_wet), ]
    return(wb)
  }) |> 
    dplyr::bind_rows()
  
  
  # wb <- obs_met |>
  #   dplyr::select(Date) |>
  # 
  #   # add inflow discharge from fwmt model
  #   merge(vol.inflow, by = "Date") |>
  #   merge(vol.outflow, by = "Date") |>
  # 
  #   # add evaporation estimation above
  #   merge(dplyr::select(wbal, c("Date", "dy_cd_evap_m3", "gotm_wet_evap_m3",
  #                               "glm_aed_evap_m3", "dy_cd_evap_flux",
  #                               "gotm_wet_evap_flux", "glm_aed_evap_flux",
  #                               "T5avg", "Ts", "area", "value", "V")),
  #         by = "Date") |>
  #   merge(dplyr::select(obs_met, c("Date","MET_pprain"))) |>
  #   # rainfall direct to surface of lake
  #   dplyr::mutate(rain = MET_pprain * area,
  #                 # calculate outflow by difference
  #                 deltaV = c(0, diff(value)) * area,
  #                 ToT_inflow = HYD_flow + rain,
  #                 outflow_dy_cd = ((HYD_flow + rain) -
  #                                    (HYD_outflow + dy_cd_evap_m3 + deltaV)),
  #                 outflow_glm_aed = ((HYD_flow + rain) -
  #                                      (HYD_outflow + glm_aed_evap_m3 + deltaV)),
  #                 outflow_gotm_wet = ((HYD_flow + rain) -
  #                                       (HYD_outflow + gotm_wet_evap_m3 + deltaV)),
  #                 sum_gotm_wet = HYD_flow + rain)
  # wb <- wb[complete.cases(wb$outflow_dy_cd), ]
  # wb <- wb[complete.cases(wb$outflow_glm_aed), ]
  # wb <- wb[complete.cases(wb$outflow_gotm_wet), ]
  
  # plot(wb$Date, wb$outflow_gotm_wet)
  # ggplot(wb) +
  #   geom_hline(yintercept = 0) +
  #   geom_point(aes(Date, outflow_gotm_wet))
  
  # Method 1 - No inflows or outflows
  if (method == 1) {
    wb <- wb |>
      dplyr::mutate(
        outflow = 0,
        inflow = 0
      )
    # dplyr::mutate(outflow_dy_cd = 0,
    #               outflow_glm_aed = 0,
    #               outflow_gotm_wet = 0,
    #               inflow_dy_cd = 0,
    #               inflow_glm_aed = 0,
    #               inflow_gotm_wet = 0)
    # Method 2 - Outflows
  } else if (method == 2) {
    # pass negative outflows onto subsequent days
    wb <- wb |>
      split(wb$model) |>
      lapply(\(df) {
        for (j in 2:nrow(df)) {
          if (df$outflow[j - 1] < 0) {
            df$outflow[j] <- df$outflow[j] + df$outflow[j - 1]
          }
        }
        df
      }) |>
      dplyr::bind_rows()
    
    # Smooth outflow by 5 days ----
    wb <- wb |>
      dplyr::group_by(model) |>
      dplyr::mutate(
        outflow = zoo::rollmean(outflow, 5, na.pad = FALSE, fill = 0,
                                align = c("right")),
        # outflow_dy_cd = zoo::rollmean(wb$outflow_dy_cd, 5, na.pad = TRUE,
        #                               align = c("right")),
        # outflow_glm_aed = zoo::rollmean(wb$outflow_glm_aed, 5, na.pad = TRUE,
        #                                 align = c("right")),
        # outflow_gotm_wet = zoo::rollmean(wb$outflow_gotm_wet, 5, na.pad = TRUE,
        #                                  align = c("right"))
      ) |>
      # remove the negatives that have been added to other days
      dplyr::mutate(
        outflow = dplyr::case_when(
          outflow < 0 ~ 0, .default = outflow
        ),
        inflow = 0
        # outflow_dy_cd = dplyr::case_when(
        #   outflow_dy_cd < 0 ~ 0, .default = outflow_dy_cd
        # ),
        # outflow_glm_aed = dplyr::case_when(
        #   outflow_glm_aed < 0 ~ 0, .default = outflow_glm_aed
        # ),
        # outflow_gotm_wet = dplyr::case_when(
        #   outflow_gotm_wet < 0 ~ 0, .default = outflow_gotm_wet
        # ),
        # inflow_dy_cd = 0,
        # inflow_glm_aed = 0,
        # inflow_gotm_wet = 0
      ) |> 
      dplyr::ungroup()
    # Method 3 - Inflows and outflows
  } else  if (method == 3) {
    # Separate negative into inflows and positive into outflows
    wb <- wb |>
      dplyr::group_by(model) |>
      # Smooth outflow by 5 days ----
    dplyr::mutate(
      outflow = zoo::rollmean(outflow, 5, na.pad = FALSE, fill = 0,
                              align = c("right"))
      # outflow_dy_cd = zoo::rollmean(wb$outflow_dy_cd, 5, na.pad = TRUE,
      #                               align = c("right")),
      # outflow_glm_aed = zoo::rollmean(wb$outflow_glm_aed, 5, na.pad = TRUE,
      #                                 align = c("right")),
      # outflow_gotm_wet = zoo::rollmean(wb$outflow_gotm_wet, 5, na.pad = TRUE,
      #                                  align = c("right"))
    ) |>
      dplyr::mutate(
        inflow = dplyr::case_when(
          outflow < 0 ~ abs(outflow),
          .default = 0
        ),
        # inflow_dy_cd = dplyr::case_when(
        #   outflow_dy_cd < 0 ~ abs(outflow_dy_cd),
        #   .default = 0
        # ),
        # inflow_glm_aed = dplyr::case_when(
        #   outflow_glm_aed < 0 ~ abs(outflow_glm_aed),
        #   .default = 0
        # ),
        # inflow_gotm_wet = dplyr::case_when(
        #   outflow_gotm_wet < 0 ~ abs(outflow_gotm_wet),
        #   .default = 0
        # ),
      ) |>
      dplyr::mutate(
        outflow = dplyr::case_when(
          outflow < 0 ~ 0, .default = outflow
        )
        # outflow_dy_cd = dplyr::case_when(
        #   outflow_dy_cd < 0 ~ 0, .default = outflow_dy_cd
        # ),
        # outflow_glm_aed = dplyr::case_when(
        #   outflow_glm_aed < 0 ~ 0, .default = outflow_glm_aed
        # ),
        # outflow_gotm_wet = dplyr::case_when(
        #   outflow_gotm_wet < 0 ~ 0, .default = outflow_gotm_wet
        # )
      ) |> 
      dplyr::ungroup()
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
  
  wb |>
    dplyr::mutate(HYD_temp = Ts, CHM_salt = 0) |>
    dplyr::select(c("Date", "model", "value", "HYD_flow", "rain",
                    evap_m3, evap_flux,
                    # "dy_cd_evap_m3", "gotm_wet_evap_m3", "glm_aed_evap_m3",
                    "deltaV", "V",
                    # "dy_cd_evap_flux", "gotm_wet_evap_flux", "glm_aed_evap_flux",
                    "Ts", "area", "CHM_salt", "HYD_temp",
                    "inflow", "outflow"
                    # "inflow_dy_cd", "inflow_glm_aed", "inflow_gotm_wet",
                    # "outflow_dy_cd", "outflow_glm_aed", "outflow_gotm_wet"
                    ))
}

#' Calculate actual surface area at a specific depth
#' @param depth numeric; depth of the lake (m)
#' @inheritParams build_dycd
#' @noRd
get_hyps_val <- function(depth, hyps) {
  sapply(depth, function(l) approx(hyps[["elev"]], hyps[["area"]], xout = l)$y)
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
#' @param mod.lvl data.frame; data.frame with Date and value columns
#' @noRd
optim_lvl_params <- function(parameters, mod.lvl, surf) {
  ampl <- parameters[1]
  offset <- parameters[2]
  
  # Call mod_lvl with the current ampl and offset values
  # Calculate the goodness of fit with your data
  predicted_values <- mod_lvl(mod.lvl$Date, surf = surf, ampl = ampl, offset = offset)
  residuals <- predicted_values - mod.lvl$value
  sum_of_squares <- sum(residuals^2, na.rm = TRUE)  # You can use a different error metric
  
  return(sum_of_squares)
}
