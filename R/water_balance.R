#' Estimate lake water balance based on a minimal set of inputs
#'
#' @param aeme_time list; time object from aeme_object using `time()`
#' @inheritParams build_ensemble
#' @param hyps data frame of hypsographic curve, elevation (masl) and planar
#' area (m^2)
#' @param inf list of inflow data frames
#' @param outf list of outflow data frames. Default = NULL
#' @param obs_lvl data frame of lake water level observations.. cols = Date,
#'  value
#' @param obs_lake data frame of lake observations in ensemble standard format
#' @param obs_met data frame of meteorology, must include MET_tmpair, MET_wndspd
#'  & MET_prvapr, continuous Date, extent defines output extent
#' @param ext_elev numeric; elevation to extend bathymetry
#' @param elevation numeric; elevation of lake
#' @param print_plots logical; print plots of water balance components
#' @param coeffs numeric vector; coefficients for estimating lake surface
#' temperature. Default = NULL
#'
#' @return data frame of water balance components which are:
#' - Date
#' - lvlwtr
#' - HYD_flow
#' -...
#'
#' @export
#'

water_balance <- function(aeme_time, model, hyps, inf, outf = NULL,
                          obs_lvl = NULL, obs_lake = NULL, obs_met, ext_elev,
                          elevation, print_plots = FALSE, coeffs = NULL) {


  # if no observations of level..
  if (is.null(obs_lvl)) {

    if ((max(hyps$elev) - min(hyps$elev)) > 50) {
      message("Depth is >50 m - amplitutde 0.1 * depth")
      ampl <- 0.01 * (max(hyps$elev) - min(hyps$elev))
    } else {
      ampl <- 0.07 * (max(hyps$elev) - min(hyps$elev))
    }

    # ampl = 0
    offset <- -1
    surf <- max(hyps[,1])

    mod.lvl <- obs_met |>
      dplyr::mutate(lvlwtr =  mod_lvl(Date, surf = max(hyps[, 1]), ampl = ampl,
                                      offset = offset)) |>
      dplyr::select(Date, lvlwtr)

  } else { # we have some level observations..


    if (nrow(obs_lvl) < 2) {
      warning("Less than 2 water level values...\nEstimating water level.")
      ampl <- 0
      offset <- -1
      surf <- max(hyps[, 1])

    } else if (nrow(obs_lvl) < 10) {

      ampl <- ((quantile(obs_lvl$lvlwtr, 0.9) -
                  quantile(obs_lvl$lvlwtr, 0.1)) / 2) |>
        as.numeric()

      offset <- -1
      surf <- max(hyps[,1])

    } else if (max(diff(obs_lvl$Date) < 10)) {

      message("Using observed water level")
      # placeholder.. add optimised sin model here..!
      ampl <- ((quantile(obs_lvl$lvlwtr,0.9) -
                  quantile(obs_lvl$lvlwtr,0.1)) / 2) |>
        as.numeric()
      offset <- -1
      surf <- median(obs_lvl$lvlwtr)
      mod.lvl <- data.frame(Date = obs_met$Date) |>
        dplyr::left_join(obs_lvl, by = "Date") |>
        dplyr::mutate(yday = lubridate::yday(Date))

      fit <- stats::loess(lvlwtr ~ yday, data = mod.lvl)
      newdata <- mod.lvl |>
        dplyr::mutate(lvlwtr = NA)
      mod.lvl$pred <- stats::predict(fit, newdata = newdata)
      # plot(mod.lvl$lvlwtr)
      # lines(mod.lvl$pred, col = 2)
      mod.lvl <- mod.lvl |>
        dplyr::mutate(lvlwtr = dplyr::case_when(
          is.na(lvlwtr) ~ pred,
          .default = lvlwtr
        )) |>
        dplyr::select(Date, lvlwtr) |>
        dplyr::mutate(lvlwtr = lvlwtr - (surf - max(hyps[,1])))


    } else {
      print("Unsure when this is used...")
      # placeholder.. add interpolation of water level record here..!
      ampl <- ((quantile(obs_lvl$lvlwtr,0.9) - quantile(obs_lvl$lvlwtr,0.1)) / 2) |> as.numeric()
      offset <- -1
      surf <- median(obs_lvl$lvlwtr)
    }
  }

  obs_met <- obs_met |>
    dplyr::mutate(MET_pprain = MET_pprain / 1000,
                  MET_ppsnow = MET_ppsnow / 1000) |> # convert to m
    dplyr::mutate(T5avg = zoo::rollmean(MET_tmpair, 5, na.pad = TRUE,
                                        align = c("right")))

  # Evaporation ----
  evap <- obs_met |>
    dplyr::select(c("Date","MET_tmpair", "MET_prvapr","MET_wndspd")) |>
    dplyr::mutate(T14avg = zoo::rollmean(MET_tmpair, 14, na.pad = TRUE,
                                         align = c("right")),
                  T5avg = zoo::rollmean(MET_tmpair, 5, na.pad = TRUE,
                                        align = c("right")))

  if (!is.null(obs_lake)) {
    # if no lake observations
    sub <- obs_lake |>
      dplyr::filter(
        var == "HYD_temp",
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
  }

  if (is.null(obs_lake)) {
    if (!is.null(coeffs)) {
      message("Estimating temperature using supplied coefficients...")
      evap$value <- coeffs[1] + coeffs[2] * evap$T5avg #
    }
  }

  # if less than 10 measurements
  if (sum(!is.na(evap[["value"]])) < 10 & is.null(coeffs)) {
    message("Estimating temperature using Stefan & Preud'homme (2007)...")
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

  # extended hypsography
  hyps.ext <- bathy_extrap(hyps, 0.75, (max(hyps$elev) + ext_elev))

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
    dplyr::mutate(precip = precip / 86400) #|>

  if (!("sst" %in% names(obs_met))) {
    gotm_met <- gotm_met |>
      dplyr::mutate(sst = airt * coeffs[2] + coeffs[1])
    obs_met <- obs_met |>
      dplyr::mutate(sst = T5avg * coeffs[2] + coeffs[1])
  }

  # gotm_evap <- calc_evap(met = gotm_met, model ="gotm_wet")
  # glm_evap <- calc_evap(met = gotm_met, elevation = elevation,
  #                       model = "glm_aed")

  # Get dates to use for calculating the water balance
  max_spin <- max(unlist(aeme_time[["spin_up"]])[model])
  spin_start <- aeme_time[["start"]] - lubridate::ddays(max_spin + 6)
  date_stop <- aeme_time[["stop"]] + lubridate::ddays(1)

  gotm_met <- gotm_met |>
    dplyr::filter(Date >= spin_start & Date <= date_stop) # filter dates

  rho0 <- 1e3
  Latent_Heat_Evap = 2.453E+6
  evap <- obs_met |>
    dplyr::left_join(mod.lvl, by = "Date") |>
    dplyr::filter(Date >= spin_start & Date <= date_stop) |> # filter dates
    # calculate the fluctuating surface area
    dplyr::mutate(lvlwtr2 = mod_lvl(Date, surf = max(hyps[,1]), ampl = ampl,
                            offset = offset)) |>
    dplyr::mutate(
      area = get_hyps_val(depth = lvlwtr, hyps = hyps.ext),
      # Calculate 5-day average water temperature
      T5avg = zoo::rollmean(MET_tmpair, 5, na.pad = TRUE, align = c("right")),
      # apply the model to predict surface temperature
      Ts = sst,
      #saturation vapor pressure
      es = exp (2.3026 *(((7.5*Ts)/(Ts+237.3)+0.7858))),
      #evaporative heat flux
      Qlh = (0.622/981.9) *         #constant/mean station pressure
              0.0013 *               #latent heat transfer coefficient
              1.168 *                #density of air
              2453000 *              #latent heat of evaporation of water
              MET_wndspd *           #wind speed in m/s
              (MET_prvapr - es),
      #change in mass of surface layer
      deltaM = ((-1 * Qlh) * area) / 2258000,
      #total evaporative loss
      evap = deltaM * 86400 / 1000,
      # Evaporation rate
      dy_cd_evap_flux = -(evap / area)/86400,
      gotm_wet_evap_flux = calc_evap(met = gotm_met, model = "gotm_wet",
                                     method = "kondo"),
      glm_aed_evap_flux = dy_cd_evap_flux,
      # glm_aed_evap_flux = calc_evap(met = gotm_met, elevation = elevation,
      #                               model = "glm_aed"),
      dy_cd_evap_m3 = -dy_cd_evap_flux * area * 86400,
      gotm_wet_evap_m3 = -gotm_wet_evap_flux * area * 86400,
      glm_aed_evap_m3 = -glm_aed_evap_flux * area * 86400,
      V = calc_V(depth = lvlwtr, hyps = hyps.ext, h = 0.01),
      evap_rate2 = Qlh / Latent_Heat_Evap / rho0,
      evap_rate3 = Qlh / Latent_Heat_Evap / wtr_density(Ts)
      ) |>
    dplyr::mutate(Qlh = dplyr::case_when(
      Qlh > 0 ~ 0,
      .default = Qlh
    ))
  # apply the functions

  # V = calc_V(depth = evap$lvlwtr, hyps = hyps)

  if (any(is.na(evap$area))) {
    stop("NA's in area. Most likely due to 'ext_elev' being too small.")
  }

  # get total inflow discharge
  if (is.null(inf) | length(inf) == 0) {
    vol.inflow <- data.frame(Date = obs_met$Date, HYD_flow = 0)
  } else {
    vol.inflow <- inf |>
      dplyr::bind_rows() |>
      dplyr::select(c("Date","HYD_flow")) |>
      dplyr::group_by(Date) |>
      dplyr::summarise(HYD_flow = sum(HYD_flow))
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
  wb <- obs_met |>
    dplyr::select(Date) |>

    # add inflow discharge from fwmt model
    merge(vol.inflow, by = "Date") |>
    merge(vol.outflow, by = "Date") |>

    # add evaporation estimation above
    # merge(., select(evap, c("Date","evap", "Ts", "area", "lvlwtr")), by = "Date") |>
    merge(dplyr::select(evap, c("Date", "dy_cd_evap_m3", "gotm_wet_evap_m3",
                                "glm_aed_evap_m3", "dy_cd_evap_flux",
                                "gotm_wet_evap_flux", "glm_aed_evap_flux", "Ts",
                                "area", "lvlwtr", "V")), by = "Date") |>
    merge(dplyr::select(obs_met, c("Date","MET_pprain"))) |>
    # rainfall direct to surface of lake
    dplyr::mutate(rain = MET_pprain * area,
           # calculate outflow by difference
           deltaV = c(0, diff(lvlwtr)) * area,
           ToT_inflow = HYD_flow + rain,
           outflow_dy_cd = ((HYD_flow + rain) -
                              (HYD_outflow + dy_cd_evap_m3 + deltaV)),
           outflow_glm_aed = ((HYD_flow + rain) -
                                (HYD_outflow + glm_aed_evap_m3 + deltaV)),
           outflow_gotm_wet = ((HYD_flow + rain) -
                                 (HYD_outflow + gotm_wet_evap_m3 + deltaV)),
           sum_gotm_wet = HYD_flow + rain)
  wb <- wb[complete.cases(wb$outflow_dy_cd), ]
  wb <- wb[complete.cases(wb$outflow_glm_aed), ]
  wb <- wb[complete.cases(wb$outflow_gotm_wet), ]

  # plot(wb$Date, wb$outflow_gotm_wet)
  # ggplot(wb) +
  #   geom_hline(yintercept = 0) +
  #   geom_point(aes(Date, outflow_gotm_wet))

  # pass negative outflows onto subsequent days
  for (j in 2:nrow(wb)) {
    wb[j,"outflow_dy_cd"] <- ifelse(wb[j-1, "outflow_dy_cd"] < 0,
                                    wb[j,"outflow_dy_cd"] +
                                      wb[j-1,"outflow_dy_cd"],
                                    wb[j,"outflow_dy_cd"])
    wb[j,"outflow_glm_aed"] <- ifelse(wb[j-1, "outflow_glm_aed"] < 0,
                                      wb[j,"outflow_glm_aed"] +
                                        wb[j-1,"outflow_glm_aed"],
                                      wb[j,"outflow_glm_aed"])
    wb[j,"outflow_gotm_wet"] <- ifelse(wb[j-1, "outflow_gotm_wet"] < 0,
                                       wb[j,"outflow_gotm_wet"] +
                                         wb[j-1,"outflow_gotm_wet"],
                                       wb[j,"outflow_gotm_wet"])
  }

  # smooth outflow by 5 days

  wb <- wb |>
    dplyr::mutate(
      outflow_dy_cd = zoo::rollmean(wb$outflow_dy_cd, 5, na.pad = TRUE,
                                    align = c("right")),
      outflow_glm_aed = zoo::rollmean(wb$outflow_glm_aed, 5, na.pad = TRUE,
                                    align = c("right")),
      outflow_gotm_wet = zoo::rollmean(wb$outflow_gotm_wet, 5, na.pad = TRUE,
                                    align = c("right"))
    ) |>
    # remove the negatives that have been added to other days
    dplyr::mutate(
      outflow_dy_cd = dplyr::case_when(
        outflow_dy_cd < 0 ~ 0, .default = outflow_dy_cd
      ),
      outflow_glm_aed = dplyr::case_when(
        outflow_glm_aed < 0 ~ 0, .default = outflow_glm_aed
      ),
      outflow_gotm_wet = dplyr::case_when(
        outflow_gotm_wet < 0 ~ 0, .default = outflow_gotm_wet
      )
    )


  if (print_plots) {
    wb |>
      tidyr::pivot_longer(cols = !contains("Date"), names_to = "var",
                          values_to = "value") |>
      # gather(var,value,2:ncol(.)) |>
      ggplot2::ggplot(ggplot2::aes(x = Date, y = value)) +
      ggplot2::geom_hline(yintercept = 0) +
      ggplot2::geom_line() +
      ggplot2::ylab(bquote('Volume ('~m^-3~d^-1~')')) +
      ggplot2::xlab("Date") +
      ggplot2::theme_bw() +
      ggplot2::facet_wrap(~var, ncol = 1, scales = 'free_y')
  }

  wb <- wb |>
    dplyr::select(c("Date", "lvlwtr", "HYD_flow", "rain", "dy_cd_evap_m3",
                    "gotm_wet_evap_m3", "glm_aed_evap_m3", "deltaV", "V",
                    "dy_cd_evap_flux", "gotm_wet_evap_flux",
                    "glm_aed_evap_flux", "Ts", "area", "outflow_dy_cd",
                    "outflow_glm_aed", "outflow_gotm_wet"))

  return(wb)
}

#' Calculate actual surface area at a specific depth
#' @param depth numeric; depth of the lake (m)
#' @inheritParams build_dycd
#' @noRd
get_hyps_val <- function(depth, hyps) {
  sapply(depth, function(l) approx(hyps[, 1], hyps[, 2], xout = l)$y)
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
    if (tail(depths, 1) != d ) {
      depths <- c(depths, d)
    }
    # areas <- approx()
    areas <- approx(hyps[["elev"]], hyps[["area"]], depths)$y
    r <- sqrt((c(areas[-length(areas)]) / pi))
    R <- sqrt((areas[-1] / pi))
    V <- ((pi * h) / 3) * (R*R + R*r + r*r)
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

  surf + ampl *(sin( ( (DOY * 2 * 3.14159/daysinyear) + (offset) ) ) )
}

#' Calculate water density
#' @param wtr numeric vector of water temperature
#' @noRd
wtr_density <- function(wtr) {
  (1000 * (1 - (wtr + 288.9414) * (wtr - 3.9863)^2/(508929.2 *
                                                      (wtr + 68.12963))))
}
