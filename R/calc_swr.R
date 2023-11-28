#' Calculate shortwave radiation
#'
#' @param time POSIXct vector of times
#' @param lat numeric; latitude
#' @param lon numeric; longitude
#' @param cloud numeric vector; cloud cover
#' @param to_hourly logical; convert daily cloud cover to hourly
#'
#' @importFrom withr local_locale local_timezone
#' @importFrom lubridate yday hour
#'
#' @return numeric vector of shortwave radiation or data.frame with hourly
#'  shortwave radiation
#' @noRd

calc_swr <- function(time, lat, lon, cloud, to_hourly  = FALSE) {

  # Set timezone temporarily to UTC
  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")

  # Input checks
  if (!is.POSIXt(time))
    stop("time must be a vector of class POSIXt")

  if (!is.numeric(lat) || !is.numeric(lon) || !is.numeric(cloud))
    stop("lat, lon, and cloud must be numeric vectors")

  if (length(lat) != 1 || length(lon) != 1)
    stop("lat and lon must be single numeric values")

  if (length(time) != length(cloud))
    stop("time and cloud must have the same length")

  if (any(cloud < 0) || any(cloud > 1))
    stop("cloud values must be in the range [0, 1]")

  if (!is.logical(to_hourly ))
    stop("to_hourly  must be a logical value")

  # Convert daily cloud cover to hourly
  if (to_hourly ) {
    time <- seq.POSIXt(from = time[1], to = (time[length(time)] + 23 * 60 * 60),
                       by = '1 hour')
    cloud <- rep(cloud, each = 24)
  }

  # Constants
  deg2rad <- pi / 180
  rad2deg <- 180 / pi
  solar <- 1350.0
  eclips <- 23.439 * deg2rad
  tau <- 0.7
  aozone <- 0.09

  # Extract day of year and hour
  yday <- lubridate::yday(time)
  hh <- lubridate::hour(time)

  # From GOTM solar_zenith_angle.F90
  rlon <- deg2rad * lon
  rlat <- deg2rad * lat

  yrdays <- 365.25
  th0 <- 2.0 * pi * yday / yrdays
  th02 <- 2.0 * th0
  th03 <- 3.0 * th0
  sundec <- 0.006918 - 0.399912 * cos(th0) + 0.070257 * sin(th0) - 0.006758 *
    cos(th02) + 0.000907 * sin(th02) - 0.002697 * cos(th03) + 0.001480 *
    sin(th03)
  thsun <- (hh - 12.0) * 15.0 * deg2rad + rlon
  coszen <- sin(rlat) * sin(sundec) + cos(rlat) * cos(sundec) * cos(thsun)
  coszen[coszen < 0] <- 0
  solar_zenith_angle <- rad2deg * acos(coszen)

  # From GOTM short_wave_radiation.F90
  coszen <- cos(deg2rad * solar_zenith_angle)
  qatten <- rep(NA, length(coszen))
  coszen[coszen <= 0] <- 0
  qatten[coszen <= 0] <- 0
  qatten[coszen > 0] <- tau ^ (1 / coszen)[coszen > 0]

  qzer <- coszen * solar
  qdir <- qzer * qatten
  qdiff <- ((1 - aozone) * qzer - qdir) * 0.5
  qtot <- qdir + qdiff

  rlon <- deg2rad * lon
  rlat <- deg2rad * lat
  yrdays <- 365.0
  eqnx <- (yday - 81.0) / yrdays * 2.0 * pi
  sunbet <- sin(rlat) * sin(eclips * sin(eqnx)) + cos(rlat) *
    cos(eclips * sin(eqnx))
  sunbet <- asin(sunbet) * rad2deg

  qshort <- qtot * (1 - 0.62 * cloud + 0.0019 * sunbet)
  qshort[which(cloud < 0.3)] <- qtot[which(cloud < 0.3)]
  qshort[solar_zenith_angle == 90] <- 0

  if (to_hourly ) {
    df <- data.frame(DateTime = time, SWR = qshort)
    return(df)
  } else {
    return(qshort)
  }
}
