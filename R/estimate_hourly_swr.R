#' Estimate hourly shortwave radiation from daily values
#'
#' @param met data.frame; must have Date and MET_radswd columns
#' @param lat numeric; latitude in decimal degrees
#' @param lon numeric; longitude in decimal degrees
#'
#' @return data frame with Date, SWR, and SWR_diff columns
#' @noRd
#'

estimate_hourly_swr <- function(met, lat, lon, cloud = 0) {

  # Argument checks
  if (!is.data.frame(met)) stop("met must be a data frame")
  if (!"Date" %in% names(met)) stop("met must have a Date column")
  if (!"MET_radswd" %in% names(met)) stop("met must have a MET_radswd column")
  if (!is.numeric(lat)) stop("lat must be numeric")
  if (!is.numeric(lon)) stop("lon must be numeric")

  time <- met$Date
  # lat <- -36.88
  # lon <- 174.469
  swr <- met$MET_radswd

  time <- seq.POSIXt(from = as.POSIXct(time[1]),
                     to = (as.POSIXct(time[length(time)]) + 23 * 60 * 60),
                     by = "1 hour")
  # cloud <- rep(0, times = length(time))

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
  sza <- solar_zenith_angle(yday, hh, lon, lat)

  swr2 <- shortwave_radiation(zenith_angle = sza, yday = yday, dlon = lon,
                              dlat = lat, cloud = cloud)

  # swr3 <- (MeTo::Rso(x = time, lat.deg = lat, long.deg = lon, elev = 0, tl = 1, control = list(Lz = 360 - lon))
  #          * 1000000) / 3600


  df_day <- data.frame(DateTime = time, SWR = swr2) |>
    dplyr::mutate(Date = as.Date(DateTime)) |>
    dplyr::group_by(Date) |>
    dplyr::summarise(SWR = mean(SWR, na.rm = TRUE)) |>
    dplyr::mutate(SWR_diff = swr / SWR)

  # Adjust the SWR in df_hr to match the daily mean
  df_hr <- data.frame(DateTime = time, SWR_hr = swr2, Date = as.Date(time)) |>
    dplyr::left_join(df_day, by = "Date") |>
    dplyr::mutate(SWR_hr = SWR_hr * SWR_diff) |>
    dplyr::select(DateTime, SWR_hr) |>
    dplyr::mutate(DateTime = format(DateTime, format = "%Y-%m-%d %H:%M:%S"))

  return(df_hr)

  # Compare daily and hourly SWR

  # df_day <- df_day |>
  #   dplyr::mutate(SWR_MJ =  (SWR * 3600 * 24) / 1000000)
  #
  # df_hr2 <- df_hr |>
  #   dplyr::mutate(SWR_MJ =  (SWR_hr * 3600) / 1000000,
  #                 Date = as.Date(DateTime)) |>
  #   dplyr::group_by(Date) |>
  #   dplyr::summarise(SWR_MJ = sum(SWR_MJ, na.rm = TRUE))
  #
  # head(df_hr2)
  # head(df_day)


}

#' Calculate solar zenith angle
#'
#' @param yday vector of day of year
#' @param hh vector of hour of day
#' @param dlon numeric; longitude in decimal degrees
#' @param dlat numeric; latitude in decimal degrees
#'
#' @return vector of solar zenith angles in decimal degrees
#' @noRd
#' @source https://github.com/gotm-model/code/blob/lake/src/airsea/solar_zenith_angle.F90
#'

solar_zenith_angle <- function(yday, hh, dlon, dlat) {

  # Constants
  deg2rad <- pi / 180
  rad2deg <- 180 / pi

  # Convert degrees to radians
  rlon <- deg2rad * dlon
  rlat <- deg2rad * dlat

  # Days per year
  yrdays <- 365.25

  # Solar time calculation
  th0 <- 2 * pi * yday / yrdays
  th02 <- 2 * th0
  th03 <- 3 * th0

  # Solar declination
  sundec <- 0.006918 - 0.399912 * cos(th0) + 0.070257 * sin(th0) -
    0.006758 * cos(th02) + 0.000907 * sin(th02) -
    0.002697 * cos(th03) + 0.001480 * sin(th03)

  # Solar hour angle
  thsun <- (hh - 12) * 15 * deg2rad + rlon

  # Cosine of the solar zenith angle
  coszen <- sin(rlat) * sin(sundec) + cos(rlat) * cos(sundec) * cos(thsun)
  # if (coszen < 0) coszen <- 0
  coszen[coszen < 0] <- 0

  # Calculate solar zenith angle in degrees
  solar_zenith_angle <- rad2deg * acos(coszen)

  return(solar_zenith_angle)
}

#' Calculate shortwave radiation
#'
#' @param zenith_angle vector of solar zenith angles in decimal degrees
#' @param yday vector of day of year
#' @param dlon numeric; longitude in decimal degrees
#' @param dlat numeric; latitude in decimal degrees
#' @param cloud vector of cloud cover fraction or scalar
#'
#' @source https://github.com/gotm-model/code/blob/lake/src/airsea/shortwave_radiation.F90
#'
#' @return vector of shortwave radiation in W m-2
#' @noRd
#'
shortwave_radiation <- function(zenith_angle, yday, dlon, dlat, cloud = 0) {

  # Constants
  deg2rad <- pi / 180
  rad2deg <- 180 / pi

  # Parameters
  solar <- 1350
  eclips <- 23.439 * deg2rad
  tau <- 0.7
  aozone <- 0.09

  # Convert zenith angle to cosine
  coszen <- cos(deg2rad * zenith_angle)

  # Calculate atmospheric attenuation
  coszen[coszen <= 0] <- 0
  qatten <- ifelse(coszen <= 0, 0, tau ^ (1 / coszen))

  # Calculate direct, diffuse, and total radiation
  qzer <- coszen * solar
  qdir <- qzer * qatten
  qdiff <- ((1 - aozone) * qzer - qdir) * 0.5
  qtot <- qdir + qdiff

  # Convert degrees to radians
  rlon <- deg2rad * dlon
  rlat <- deg2rad * dlat

  # Days per year
  yrdays <- 365

  # Calculate solar noon altitude
  eqnx <- (yday - 81) / yrdays * 2 * pi
  sunbet <- asin(sin(rlat) * sin(eclips * sin(eqnx)) + cos(rlat) * cos(eclips * sin(eqnx))) * rad2deg

  # Calculate shortwave radiation
  qshort <- qtot * (1 - 0.62 * cloud + 0.0019 * sunbet)
  qshort[qshort > qtot] <- qtot[qshort > qtot]

  return(qshort)
}


