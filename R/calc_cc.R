#' Calculate cloud cover
#'
#' Calculate cloud cover using latitude, air temperature, relative humidity (or dewpoint temperature) and short wave radiation using the calculations from Martin and McCutcheon (1999).
#'
#' @param date vector; Dates in as.POSixct class
#' @param airt vector; Air temperature values which correspond to the vector of dates
#' @param relh vector; Relative humidity values which correspond to the vector of dates
#' @param dewt vector; Dewpoint temperature values which correspond to the vector of dates. Used instead of relative humidity
#' @param swr vector; Short-wave radiation values which correspond to the vector of dates
#' @param lat numeric; Latitude position (in decimal)
#' @param lon numeric; Longitude position (in decimal)
#' @param elev numeric; elevation in metres above sea level
#' @param daily deprecated; logical; Is the data on a daily timestep. Defaults to FALSE
#' @return vector of cloud cover values which correspond to the vector of dates supplied
#' @source https://github.com/aemon-j/gotmtools/blob/yaml/R/calc_cc.R
#' @examples
#' \dontrun{
#'  met_file <- system.file('extdata/met_file.dat', package = 'GOTMr')
#'  swr_file <- system.file('extdata/swr_input_file.dat', package = 'GOTMr')
#'  met <- read.delim(met_file)
#'  met[,1] <- as.POSIXct(met[,1], tz = 'UTC')
#'  swr <- read.delim(swr_file)
#'  swr[,1] <- as.POSIXct(swr[,1], tz = 'UTC')
#'  met <- merge(met, swr, by = 1)
#'  cc <- calc_cc(date = met[,1], airt = met$AirT, dewt = met$DewT, swr = met$SWR, lat = 53, lon = -9.5, elev = 14, daily = F)
#'  plot(cc)
#' }
#' @importFrom stats aggregate
#' @importFrom zoo na.approx
#' @importFrom lubridate hours hour yday
#' @importFrom withr local_locale local_timezone
#' @noRd

calc_cc <- function(date, airt, relh = NULL, dewt = NULL, swr, lat, lon, elev, 
                    daily = FALSE) {
  
  # Set timezone temporarily to UTC
  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")
  
  orig_date <- date
  timestep <- difftime(orig_date[2], orig_date[1], units = "secs")
  
  # If the time step is 24 hours or more, create artificial hourly time steps
  if (timestep >= as.difftime(24, units = "hours")) {
    date <- seq.POSIXt(
      from = date[1],
      to   = date[length(date)] + timestep - lubridate::hours(1),
      by   = "1 hour"
    )
  }
  
  yday <- lubridate::yday(date)
  hour <- lubridate::hour(date)
  hour[hour == 0] <- 24
  
  std.mer <- seq(-90, 90, 15)
  Lsm <- std.mer[which.min(abs(lon - std.mer))]  # Local standard meridian
  
  Hsc <- 1390
  cd  <- 0.06
  Rg  <- 0.045
  
  theta <- lat * pi / 180
  
  r <- 1 + 0.017 * cos((2*pi/365)*(186 - yday))
  d <- 23.45 * pi/180 * cos((2*pi/365)*(172 - yday))
  
  dts <- (1/15) * (Lsm - lon)
  
  value <- (sin(theta)*sin(d)) / (cos(theta)*cos(d))
  tss <- (12/pi) * acos(-value) + dts + 12
  tsu <- -tss + 2*dts + 24
  
  gamma <- as.numeric(hour > tsu & hour < tss)
  
  ## Hour angle computations (hb, he)
  transform_angle <- function(h, dts, is_pm) {
    ang <- (pi/12) * (h - dts)
    ang[!is_pm] <- ang[!is_pm] + pi
    ang[is_pm]  <- ang[is_pm]  - pi
    
    ang[ang > 2*pi] <- ang[ang > 2*pi] - 2*pi
    ang[ang < 0]    <- ang[ang < 0]    + 2*pi
    ang
  }
  
  is_pm <- hour > 12
  hb <- transform_angle(hour - 1, dts, is_pm)
  he <- transform_angle(hour,     dts, is_pm)
  
  Ho <- Hsc/(r^2) * (
    sin(theta)*sin(d) +
      (12/pi) * cos(theta)*cos(d) * (sin(he) - sin(hb))
  ) * gamma
  
  ## Radiation scattering and absorption
  w <- (he + hb)/2
  alpha1 <- abs(sin(theta)*sin(d) + cos(theta)*cos(d)*cos(w))
  alpha <- atan(alpha1 / sqrt(1 - alpha1^2))
  
  theta_am <- ((288 - 0.0065*elev)/288)^5.256 / (
    sin(alpha) + 0.15 * ((alpha*180/pi) + 3.855)^(-1.253)
  )
  
  ## Dewpoint
  if (is.null(dewt)) {
    if (any(relh <= 0 | relh > 100)) {
      stop("Relative humidity must be between 0 and 100%.")
    }
    log_rh <- log(relh/100)
    dewt <- 243.04 * (log_rh + (17.625*airt)/(243.04 + airt)) /
      (17.625 - log_rh - ((17.625*airt)/(243.04 + airt)))
  }
  
  ## Inflate dewt if timestep > 2h
  if (timestep >= as.difftime(2, units = "hours")) {
    date_hr <- data.frame(date = date)
    dewt_df <- data.frame(date = as.POSIXct(orig_date), dewt = dewt)
    dewt <- dplyr::left_join(date_hr, dewt_df, by = "date") |> 
      # Fill NA down
      tidyr::fill(dewt, .direction = "down") |> 
      dplyr::pull(dewt)
    
  }
  
  Pwc <- 0.85 * exp(0.11 + 0.0614 * dewt)
  
  a2 <- exp(-(0.465 + 0.134*Pwc) * (0.179 + 0.421*exp(-0.721*theta_am)) * theta_am)
  a1 <- exp(-(0.465 + 0.134*Pwc) * (0.129 + 0.171*exp(-0.88*theta_am)) * theta_am)
  
  at <- (a2 + 0.5*(1 - a1 - cd)) / (1 - 0.5*Rg*(1 - a1 - cd))
  
  Ho <- at * Ho
  Ho[Ho < 0] <- 1
  
  df <- data.frame(DateTime = date, Ho = Ho)
  
  if (timestep >= as.difftime(2, units = "hours")) {
    df <- df |> 
      dplyr::mutate(Date = as.Date(DateTime)) |>
      dplyr::group_by(Date) |>
      dplyr::summarise(Ho = mean(Ho, na.rm = TRUE)) 
  }
  df[!df$Date %in% as.Date(orig_date), ]
  
  df$swr <- swr
  df$ccsim <- ifelse(df$Ho < df$swr, NaN, sqrt((1 - df$swr/df$Ho) / 0.65))
  df$ccsim[df$ccsim > 1] <- 1
  
  ## Gap filling
  ccsim <- df$ccsim
  good <- which(!is.nan(ccsim))
  
  if (length(good) > 1) {
    sta <- min(good)
    stp <- max(good)
    
    ccsim[sta:stp] <- zoo::na.approx(ccsim[sta:stp])
    
    if (sta > 1) ccsim[1:sta] <- ccsim[sta]
    if (stp < length(ccsim)) ccsim[stp:length(ccsim)] <- ccsim[stp]
  }
  
  ccsim
}

