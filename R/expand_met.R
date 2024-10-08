#' Expand a minimal set of meteorology inputs to a complete set of variables
#' suitable for all models
#'
#' @param met data frame of source meteorology variables
#' @param lat numeric; latitude
#' @param lon numeric; longitude
#' @param elev numeric; elevation
#' @param print.plot logical; print the plot
#'
#'
#' @importFrom psychrolib SetUnitSystem GetStationPressure GetSeaLevelPressure
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot geom_line aes facet_wrap labs theme_bw
#' @importFrom withr local_locale local_timezone
#'
#' @export
#'
#' @return dataframe with expanded met variables
#'

expand_met <- function(met, lat, lon, elev, print.plot = FALSE) {

  # Set timezone temporarily to UTC
  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")

  # station vs sea level pressures
  psychrolib::SetUnitSystem("SI")

  # check which have been supplied
  vars.all =  c("Date",
                "radswd", "radlwd", "cldcvr", "tmpair",
                "airmax", "airmin", "dewmax", "dewmin",
                "humrel", "tmpdew", "prvapr",
                "prsttn", "prmslp",
                "wndspd", "wnduvu", "wnduvv", "wnddir",
                "pprain", "ppsnow")
  for(i in vars.all) {assign(paste0("is.", i), any(grepl(i,colnames(met))))}

  # check for compulsory variables
  for (i in c("radswd", "tmpair", "pprain")) {
    # check that the var is in the met df
    if(isFALSE(get(paste0("is.",i)))) { stop(paste0(i, " is required but not
                                                    present"))}
  }

  Date = met$Date
  radswd = met[,which(grepl("radswd",colnames(met)))]
  tmpair = met[,which(grepl("tmpair",colnames(met)))]
  pprain = met[,which(grepl("pprain",colnames(met)))]

  #---- process vapour pressure, humidity, wet bulb etc


  # if no relative humidity supplied
  if (!is.humrel) {

    # check that dewpoint temperature is supplied
    if (!is.tmpdew) { stop("humrel (RH %) is not supplied, therefore, tmpdew
                           (dewpoint temperature) is required, but not
                           present")}

    tmpdew <- met[,which(grepl("tmpdew", colnames(met)))]

    # estimate humidity from wet bulb temperature,
    # see: https://www.omnicalculator.com/physics/relative-humidity
    humrel <- 100 * (exp(17.625 * tmpdew/(243.04 + tmpdew)) /
                       exp(17.625 * tmpair/(243.04 + tmpair)))
  } else {
    humrel <- met[,which(grepl("humrel",colnames(met)))]
  }


  # if no dewpoint is supplied
  if (!is.tmpdew) {

    # check that relative humidity is supplied
    if(!is.humrel) { stop("tmpdew (dewpoint temperature) is not supplied,
                          therefore, humrel (RH %) is required, but not
                          present")}

    humrel <- met[, which(grepl("humrel",colnames(met)))]

    # estimate wet bulb temperature from humidity
    # see https://www.omnicalculator.com/physics/dew-point (Lawrence2005) with an uncertainty of 0.35 °C
    alpha_t_rh <- log(humrel/100) + (17.625*tmpair)/(243.04+tmpair)
    tmpdew <- (243.04 * alpha_t_rh) / (17.625 - alpha_t_rh)
  } else {
    tmpdew <- met[, which(grepl("tmpdew", colnames(met)))]
  }

  # if no vapour pressure is supplied
  if (!is.prvapr) {
    # get vapour pressure per (Eqn. C2, TVA,1972) via DYRESM manual
    prvapr <- (humrel / 100) *
      exp(2.303 * ((7.5 * tmpair / (tmpair + 237.3)) + 0.7858))
  } else {
    prvapr <- met[, which(grepl("prvapr", colnames(met)))]
  }


  # if no cloud cover is supplied
  if (!is.cldcvr) {
    cldcvr <- calc_cc(date = as.POSIXct(Date), airt = tmpair, relh = humrel,
                      swr = radswd, lat = lat, lon = lon,
                      elev = elev)

  } else {
    cldcvr = met[, which(grepl("cldcvr", colnames(met)))]
  }



  # if no station pressure
  if (!is.prsttn) {

    # check that msl pressure is supplied
    if (!is.prmslp) { stop("prsttn (station level pressure hPa) is not supplied,
                           therefore, prmslp (mean sea level pressure hPa) is
                           required, but not present")}

    prmslp <- met[, which(grepl("prmslp",colnames(met)))]

    # estimate station pressure from sea level pressure
    prsttn <- psychrolib::GetStationPressure(prmslp * 100, Altitude = elev,
                                             tmpair)/100

  } else {
    prsttn <- met[, which(grepl("prsttn",colnames(met)))]
  }

  # if no msl pressure
  if (!is.prmslp) {

    # check that msl pressure is supplied
    if (!is.prsttn) {
      stop("prmslp (mean sea level pressure hPa) is not supplied, therefore, prsttn (station level pressure hPa) is required, but not present")
    }

    # estimate station pressure from sea level pressure
    prmslp <- psychrolib::GetSeaLevelPressure(prsttn * 100, Altitude = elev,
                                              tmpair)/100
  } else {
    prmslp <- met[, which(grepl("prmslp",colnames(met)))]
  }


  # if wind speed
  if (is.wndspd) {
    wndspd <- met[,which(grepl("wndspd",colnames(met)))]
  }
  if (is.wnddir) {
    wnddir <- met[,which(grepl("wnddir",colnames(met)))]
  }
  # if there are vector U and V components
  if (is.wnduvu & is.wnduvu) {

    # check that wind U and V are supplied
    if (!is.wnduvu | !is.wnduvu) {
      stop("wndspd (wind speed m/s) is not supplied, therefore, wnduvu & wnduvv
           (wind vector U & V components m/s) are required, but not present")
    }

    wnduvu <- met[, which(grepl("wnduvu", colnames(met)))]
    wnduvv <- met[, which(grepl("wnduvv", colnames(met)))]

    # get wind speed from vector components
    wnd_ds <- uv2ds(wnduvu, wnduvv)
    wndspd <- wnd_ds[, 2]
    wnddir <- wnd_ds[, 1]
    is.wndspd <- TRUE
    is.wnddir <- TRUE

  }

  # check that wind speed and dir are supplied
  if(!is.wnddir) {
    wnddir <- rep(180, length(wndspd))
  }

  # if no vector U and V components
  # if (is.wnduvu & is.wnduvu) {
  #
  #   wnduvu <- met[, which(grepl("wnduvu",colnames(met)))]
  #   wnduvv <- met[, which(grepl("wnduvv",colnames(met)))]
  #
  #   if(!is.wnddir) {
  #     # wnddir <- uv2ds(u = wnduvu, v = wnduvv)[, 1]
  #     # is.wnddir <- TRUE
  #   } else {
  #     # adjust wind direction to represent direction to (not from)
  #     wnddir <- met[,which(grepl("wnddir", colnames(met)))]
  #     wnddir <- ifelse((wnddir + 180) > 360, wnddir - 180, wnddir + 180)
  #   }
  #   # get wind speed from vector components
  #   wndspd <- uv2ds(wnduvu, wnduvv)[, 2]
  #   is.wndspd <- TRUE
  #
  # }



  if(!is.wndspd) {
    stop(strwrap("wnduvu & wnduvv wind vector components not supplied,
    therefore, wndspd (& ideally direction) are required, but not present",
                 width = 80))
  }
  if (!is.wnduvu & !is.wnduvu) {

    u_v <- ds2uv(d = wnddir, s = wndspd)
    wnduvu <- u_v[, "v"]
    wnduvv <- u_v[, "u"]
  }

  if (!is.radlwd) {
    radlwd <- calc_in_lwr(cc = cldcvr, airt = tmpair, relh = humrel)
  } else {
    radlwd <- met[, which(grepl("radlwd",colnames(met)))]
  }

  # add snow if needs be
  if (!is.ppsnow) {
    ppsnow <- rep(0, length(tmpair))
  } else {
    ppsnow <- met[,which(grepl("ppsnow",colnames(met)))]
  }

  # max/min
  if (!is.airmax) {
    airmax <- 0
  } else {
    airmax <- met[,which(grepl("airmax",colnames(met)))]
  }
  if (!is.airmin) {
    airmin <- 0
  } else {
    airmin <- met[,which(grepl("airmin",colnames(met)))]
  }
  if (!is.dewmax) {
    dewmax <- 0
  } else {
    dewmax <- met[,which(grepl("dewmax",colnames(met)))]
  }
  if (!is.dewmin) {
    dewmin <- 0
  } else {
    dewmin <- met[,which(grepl("dewmin",colnames(met)))]
  }

  out <- data.frame(Date = Date,
                    radswd, radlwd, cldcvr, tmpair,
                    airmax, airmin, dewmax, dewmin,
                    humrel, tmpdew, prvapr,
                    prsttn, prmslp,
                    wndspd, wnddir, wnduvu, wnduvv,
                    pprain, ppsnow)
  colnames(out) <- (c("Date", paste0("MET_", colnames(out)[2:ncol(out)])))

  # Round to 2 decimal places
  out[, -1] <- round(out[, -1], 3)


  if (print.plot) {
    # plot to check final frame
    p <- out |>
      tidyr::pivot_longer(cols = !dplyr::contains(Date), names_to = "var",
                          values_to = "value") |>
      ggplot2::ggplot() +
      ggplot2::geom_line(ggplot2::aes(Date, value)) +
      ggplot2::facet_wrap(~var, ncol = 2, scales= "free") +
      ggplot2::labs(x = NULL, y = "Value") +
      ggplot2::theme_bw()
    print(p)
  }

  return(out)

}

