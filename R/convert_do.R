#' Convert dissolved oxygen between mg/L and percent saturation
#'
#' @param value numeric vector of dissolved oxygen values to convert
#' @param depth depth (m) at which the DO measurement was made
#' @param temp water temperature (°C)
#' @param baro barometric pressure (mb)
#' @param altitude altitude (m). Only used if `baro` is missing.
#' @param salinity salinity (ppt). Default is 0 for freshwater.
#' @param model character, solubility model to use. Options are
#' "garcia", "garcia_benson", "weiss", or "benson".
#' @param direction character, conversion direction. Options are
#' "to_mgL" (percent saturation to mg/L) or "to_percent" (mg/L to percent
#' saturation).
#'
#' @returns numeric vector of converted dissolved oxygen values
#' @export
#'

convert_do <- function(value, depth, temp, baro, altitude = 0, salinity = 0, 
                       model = "garcia_benson", direction = "to_mgL") {
  sat_ref <- o2_at_sat(temp, depth = depth, baro = baro, salinity = salinity)
  if (direction == "to_mgL") {
    (value / 100) * sat_ref
  } else if (direction == "to_percent") {
    (value / sat_ref) * 100
  } else {
    stop("direction must be 'to_mgL' or 'to_percent'")
  }
}


#' Estimate oxygen saturation concentration
#'
#' @param temp water temperature (°C)
#' @param baro barometric pressure (mb)
#' @param altitude altitude (m). Only used if `baro` is missing.
#' @param salinity salinity (ppt).
#' @param model character, solubility model to use. Options are
#'  "garcia", "garcia_benson", "weiss", or "benson".
#'
#' @returns numeric vector of oxygen saturation concentrations (mg/L)
#' @export
#' @references
#'
#' Colt, John. \emph{1 - Solubility of Atmospheric Gases in Freshwater.} In
#' Computation of Dissolved Gas Concentration in Water as Functions of
#' Temperature, Salinity and Pressure (Second Edition), edited by John Colt,
#' 1-71. London: Elsevier, 2012.
#' http://www.sciencedirect.com/science/article/pii/B9780124159167000012.
#'
#' Garcia, H., and L. Gordon (1992), \emph{Oxygen solubility in seawater: Better
#' fitting equations}, Limnol. Oceanogr., 37(6).
#'
#' Benson, B. B. & Krause, D. (1984). \emph{The concentration and isotopic
#' fractionation of oxygen dissolved in freshwater and seawater in equilibrium
#' with the atmosphere.} Limnology and Oceanography, 29(3), 620-632.
#' doi:10.4319/lo.1984.29.3.0620
#'
#' Staehr, Peter A., Darren Bade, Matthew C. Van de Bogert, Gregory R. Koch,
#' Craig Williamson, Paul Hanson, Jonathan J. Cole, and Tim Kratz. \emph{Lake
#' Metabolism and the Diel Oxygen Technique: State of the Science.} Limnology
#' and Oceanography: Methods 8, no. 11 (November 1, 2010): 628-44.
#' doi:10.4319/lom.2010.8.0628
#'
#' USGS. \emph{New Tables of Dissolved Oxygen Saturation Values.} Quality of
#' Water Branch, 1981. http://water.usgs.gov/admin/memo/QW/qw81.11.html.
#'
#' USGS. \emph{New Tables of Dissolved Oxygen Saturation Values; Amendment of
#' Quality of Water Technical Memorandum No. 81.11.} Quality of Water Branch,
#' 1981. http://water.usgs.gov/admin/memo/QW/qw81.15.html.
#'
#' USGS. \emph{Change to Solubility Equations for Oxygen in Water.} Technical
#' Memorandum 2011.03. USGS Office of Water Quality, 2011.
#'
#' Weiss, R. (1970). \emph{The solubility of nitrogen, oxygen and argon in water
#' and seawater}. Deep Sea Research and Oceanographic Abstracts, 17(4), 721-735.
#' doi:10.1016/0011-7471(70)90037-9

o2_at_sat <- function(temp, depth, baro, altitude = 0,
                      salinity = rep(0, length(temp)),
                      model = "garcia_benson") {
  
  # Conversion from mL/L (the usual output of the garcia, weiss, etc. equations)
  # to mg/L per USGS memo 2011.03
  mgL_mlL <- 1.42905
  
  # Conversion constants
  mmHg_mb <- 0.750061683  # mm Hg to millibars
  
  # --- Barometric correction ---
  if (missing(baro)) {
    mmHg_inHg <- 25.3970886  # inches Hg → mm Hg
    standard_pressure_sea_level <- 29.92126  # Pb, inches Hg
    standard_temperature_sea_level <- 15 + 273.15  # 15 °C in K
    gravitational_acceleration <- 9.80665  # m/s^2
    air_molar_mass <- 0.0289644  # kg/mol
    universal_gas_constant <- 8.31447  # N*m/(mol*K)
    
    baro <- (1 / mmHg_mb) * mmHg_inHg * standard_pressure_sea_level |>
      (\(x) x * exp(
        (-gravitational_acceleration * air_molar_mass * altitude) /
          (universal_gas_constant * standard_temperature_sea_level)
      ))()
  }
  
  # hydrostatic pressure in mb
  rho_water <- 1000       # kg/m^3
  g <- 9.80665            # m/s^2
  hydrostatic_mb <- rho_water * g * depth / 100  # Pa → mb (Pa/100)
  
  baro <- baro + hydrostatic_mb
  
  # --- Vapor pressure & pressure correction ---
  u <- 10^(8.10765 - 1750.286 / (235 + temp))  # vapor pressure of water
  press_corr <- (baro * mmHg_mb - u) / (760 - u)
  
  # --- Select solubility model ---
  model <- tolower(model)
  
  o2_sat <- switch(
    model,
    "garcia" = {
      Ts <- log((298.15 - temp) / (273.15 + temp))
      lnC <- 2.00856 + 3.22400 * Ts + 3.99063 * Ts^2 + 4.80299 * Ts^3 +
        9.78188e-1 * Ts^4 + 1.71069 * Ts^5 -
        salinity * (6.24097e-3 + 6.93498e-3 * Ts +
                      6.90358e-3 * Ts^2 + 4.29155e-3 * Ts^3) -
        3.1168e-7 * salinity^2
      exp(lnC)
    },
    "garcia_benson" = {
      Ts <- log((298.15 - temp) / (273.15 + temp))
      lnC <- 2.00907 + 3.22014 * Ts + 4.05010 * Ts^2 + 4.94457 * Ts^3 -
        2.56847e-1 * Ts^4 + 3.88767 * Ts^5 -
        salinity * (6.24523e-3 + 7.37614e-3 * Ts +
                      1.03410e-2 * Ts^2 + 8.17083e-3 * Ts^3) -
        4.88682e-7 * salinity^2
      exp(lnC)
    },
    "weiss" = {
      temp_k <- temp + 273.15
      lnC <- -173.4292 + 249.6339 * (100 / temp_k) +
        143.3483 * log(temp_k / 100) - 21.8492 * (temp_k / 100) +
        salinity * (-0.033096 + 0.014259 * (temp_k / 100) -
                      0.0017000 * (temp_k / 100)^2)
      exp(lnC)
    },
    "benson" = {
      if (!all(salinity == 0))
        warning("Benson model does not currently include salinity")
      o2_sat <- (-0.00006 * temp^3) + (0.00725 * temp^2) -
        (0.39571 * temp) + 14.59030
      o2_sat / mgL_mlL  # undo conversion; Benson model predicts mg/L
    },
    stop(paste0("unrecognized model: ", model))
  )
  
  o2_sat * mgL_mlL * press_corr
}
