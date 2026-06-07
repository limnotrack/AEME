#' Calculate humidity-related variables using GOTM formulas
#'
#' @param hum_method Method for humidity input:
#'  1 = relative humidity (%)
#'  2 = wet bulb temperature (degC or K)
#'  3 = dew point temperature (degC or K)
#'  4 = specific humidity (kg/kg)
#' @param hum Humidity input (depends on method)
#' @param airp Air pressure (Pa)
#' @param tw Sea surface (water) temperature (degC or K)
#' @param ta Air temperature (degC or K)
#' @param rgas Gas constant for dry air (default 287.05 J/kg/K)
#' @param kelvin Value to convert degC to K (default 273.15)
#' @param const06 Constant for specific humidity conversion (default 0.62198)
#'
#' @returns List with:
#' \item{es}{Saturation vapour pressure at sea surface temperature (Pa)}
#' \item{qs}{Saturation specific humidity at sea surface temperature (kg/kg)}
#' \item{ea}{Actual vapour pressure (Pa)}
#' \item{qa}{Actual specific humidity (kg/kg)}
#' \item{rhoa}{Air density (kg/m3)}
#' @export
#'
calc_humidity_vars <- function(
    hum_method,
    hum,    # humidity input (depends on method)
    airp,   # air pressure (Pa)
    tw,     # sea surface (water) temperature (degC or K)
    ta,     # air temperature (degC or K)
    rgas = 287.05,
    kelvin = 273.15,
    const06 = 0.62198 # usually 1 for specific humidity conversion
) {
  # Polynomial coefficients from GOTM
  a1 <- 6.107799961
  a2 <- 4.436518521e-1
  a3 <- 1.428945805e-2
  a4 <- 2.650648471e-4
  a5 <- 3.031240396e-6
  a6 <- 2.034080948e-8
  a7 <- 6.136820929e-11
  
  # ensure tw, ta in degC for polynomial (if >100, assume Kelvin)
  if (tw > 100) tw_c <- tw - kelvin else tw_c <- tw
  if (ta > 100) ta_c <- ta - kelvin else ta_c <- ta
  
  # SATURATION VAPOUR PRESSURE AT SST (Pa)
  es_mb <- a1 + tw_c*(a2 + tw_c*(a3 + tw_c*(a4 + tw_c*(a5 + tw_c*(a6 + tw_c*a7)))))
  es <- es_mb * 100.0
  es <- 0.98 * es  # salt correction
  qs <- const06 * es / (airp - 0.377*es)
  
  # Now compute actual air vapour pressure + specific humidity
  if (hum_method == 1) {
    # relative humidity (%)
    rh <- 0.01 * hum
    ea_mb <- a1 + ta_c*(a2 + ta_c*(a3 + ta_c*(a4 + ta_c*(a5 + ta_c*(a6 + ta_c*a7)))))
    ea <- ea_mb * 100.0
    ea <- rh * ea
    qa <- const06*ea/(airp - 0.377*ea)
  } else if (hum_method == 2) {
    # wet bulb temperature method
    # convert hum input to wet bulb celsius
    if (hum < 100) twet_c <- hum else twet_c <- hum - kelvin
    # saturation vapor pressure at wet bulb
    ea_mb <- a1 + twet_c*(a2 + twet_c*(a3 + twet_c*(a4 + twet_c*(a5 + twet_c*(a6 + twet_c*a7)))))
    ea <- ea_mb * 100.0
    # psychrometer correction (Smithsonian met tables)
    ea <- ea - 6.6e-4*(1 + 1.15e-3*twet_c)*airp*(ta_c - twet_c)
    qa <- const06*ea/(airp - 0.377*ea)
  } else if (hum_method == 3) {
    # dew point temperature
    if (hum < 100) dew_c <- hum else dew_c <- hum - kelvin
    ea_mb <- a1 + dew_c*(a2 + dew_c*(a3 + dew_c*(a4 + dew_c*(a5 + dew_c*(a6 + dew_c*a7)))))
    ea <- ea_mb * 100.0
    qa <- const06*ea/(airp - 0.377*ea)
  } else if (hum_method == 4) {
    # specific humidity given
    qa <- hum
    ea <- qa * airp / (const06 + 0.378*qa)
  } else {
    stop("Invalid hum_method")
  }
  
  rhoa <- airp / (rgas * ((ta > 100)* (ta) + (ta <= 100)* (ta + kelvin)) * (1.0 + const06*qa))
  
  return(list(es = es, qs = qs, ea = ea, qa = qa, rhoa = rhoa))
}

#' Add humidity variables to meteorological data frame
#' @param data Data frame with meteorological data including columns:
#' "hum" (humidity input), "airp" (air pressure, Pa), "sst" (sea surface
#' temperature, degC or K), "airt" (air temperature, degC or K)
#' @param hum_method Method for humidity input:
#'  1 = relative humidity (%)
#'  2 = wet bulb temperature (degC or K)
#'  3 = dew point temperature (degC or K)
#'  4 = specific humidity (kg/kg)
#' @returns Data frame with added columns:
#' \item{es}{Saturation vapour pressure at sea surface temperature (Pa)}
#' \item{qs}{Saturation specific humidity at sea surface temperature (kg/kg)}
#' \item{ea}{Actual vapour pressure (Pa)}
#' \item{qa}{Actual specific humidity (kg/kg)}
#' \item{rhoa}{Air density (kg/m3)}
#' @export
#' @importFrom dplyr mutate select rowwise ungroup
#'
add_hum_vars <- function(data, hum_method = 1) {
  data |>
    dplyr::rowwise() |>
    dplyr::mutate(
      hum_vars = list(
        calc_humidity_vars(
          hum_method = hum_method,
          hum = hum,
          airp = airp,
          tw = sst,
          ta = airt
        )
      ),
      es   = hum_vars$es,
      qs   = hum_vars$qs,
      ea   = hum_vars$ea,
      qa   = hum_vars$qa,
      rhoa = hum_vars$rhoa
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-hum_vars)
}

