#' @title Calculate Fairall et al 1996 bulk fluxes
#'
#' @description Calculate airsea fluxes using Fairall et al 1996
#'
#' @param sst Sea surface temperature [degC]
#' @param airt Air temperature [degC]
#' @param u10 Eastward wind speed at 10m [m/s]
#' @param v10 Northward wind speed at 10m [m/s]
#' @param airp Air pressure [Pa]
#' @param hum Relative humidity [%]
#' @param precip Precipitation rate [m/s]
#' @param rain_impact Logical, should rain impact be included? Default is TRUE
#' @param calc_evaporation Logical, should evaporation be calculated? Default is TRUE
#'
#' @return List with the following elements:
#' - tau_x - Eastward wind stress [N/m^2]
#' - tau_y - Northward wind stress [N/m^2]
#' - h - Sensible heat flux [W/m^2]
#' - e - Latent heat flux [W/m^2]
#' - evap - Evaporation rate [m/s]
#'
#' @noRd
#'

# Vectorised / safer Fairall-style flux calculator (R)
# - Inputs may be scalars or vectors; they will be recycled like base R
# - Returns a data.frame with columns: taux, tauy, qe, qh, evap
# Units expected (important):
#   sst, airt      : degrees Celsius (°C). If > 100 assumed Kelvin and converted internally.
#   u10, v10       : m/s
#   airp           : Pascals (Pa). If < 2000, assumed in hPa (mb) and converted to Pa.
#   hum            : relative humidity in percent (0 - 100)
#   precip         : m/s (kg m^-2 s^-1). If very large (e.g. mm/h), you'll need to convert externally.
# Options:
#   rain_impact: apply rain-related corrections (TRUE/FALSE)
#   calc_evaporation: compute evaporation term (TRUE/FALSE)

# R version of fairall.F90 bulk air-sea flux algorithm
# Depends on a helper for saturation specific humidity (qs) & air density 
# (rhoa); these should be provided externally.

calc_fairall_vec <- function(
    sst,        # sea surface temperature (C or K)
    airt,       # air temperature (C or K)
    u10, v10,   # 10 m wind components (m/s)
    precip,     # precipitation (m/s)
    qa,         # specific humidity at measurement height
    qs,         # saturation specific humidity at sea surface
    rhoa,       # air density (kg/m3)
    rgas = 287.1,       # gas constant for dry air
    cpa = 1008,        # heat capacity of air at constant pressure
    cpw = 3985,        # heat capacity of water
    rain_impact = TRUE,    # include rain modifications
    calc_evaporation = TRUE, # include evaporation calc
    kelvin = 273.15,
    const06 = 0.62198,         # constant from Fortran (often 0)
    g = 9.81,
    kappa = 0.41
) {
  
  # roughness Reynolds numbers (Liu lookup)
  Liu_Rr <- c(0.0, 0.11, 0.825, 3.0, 10.0, 30.0, 100.0, 300.0, 1000.0)
  Liu_a <- matrix(c(
    0.177, 1.376, 1.026, 1.625,
    4.661, 34.904, 1667.19, 588000,
    0.292, 1.808, 1.393, 1.956,
    4.994, 30.709, 1448.68, 298000
  ), ncol = 2)
  Liu_b <- matrix(c(
    0, 0.929, -0.599, -1.018,
    -1.475, -2.067, -2.907, -3.935,
    0, 0.826, -0.528, -0.87,
    -1.297, -1.845, -2.682, -3.616
  ), ncol = 2)
  
  # Initialize
  w <- sqrt(u10^2 + v10^2)
  # Handle temp units
  if (sst < 100) {
    tw <- sst
    tw_k <- sst + kelvin
  } else {
    tw <- sst - kelvin
    tw_k <- sst
  }
  if (airt < 100) {
    ta_k <- airt + kelvin
    ta <- airt
  } else {
    ta_k <- airt
    ta <- airt - kelvin
  }
  
  delw <- sqrt(w^2)
  # zero outputs
  taux <- 0; tauy <- 0; qe <- 0; qh <- 0; evap <- 0
  
  if (delw != 0) {
    # kinematic viscosity of dry air
    vis_air <- 1.326e-5 * (1 + ta * (6.542e-3 + ta * (8.301e-6 - 4.84e-9 * ta)))
    # latent heat of vaporization
    L <- (2.501 - 0.00237 * tw) * 1e6
    
    delq <- qa - qs
    delt <- ta - tw
    
    # initial similarity scales
    Zw <- 10
    zt <- 2; zq <- 2
    Zabl <- 600
    Wstar <- 0.04 * delw
    Tstar <- 0.04 * delt
    Qstar <- 0.04 * delq
    TVstar <- Tstar * (1 + 0.61 * qa) + 0.61 * ta_k * Qstar
    
    ri <- g * Zw * (delt + 0.61 * ta_k * delq) / (ta_k * delw^2)
    
    if (ri <= 0.25) {
      itermax <- 20
      ier <- 0
      
      for (iter in 1:itermax) {
        if (ier >= 0) {
          oL <- g * kappa * TVstar / (ta_k * (1 + 0.61 * qa) * Wstar^2)
          ZWoL <- Zw * oL
          ZToL <- zt * oL
          ZQoL <- zq * oL
          
          wpsi <- psi_func(1, ZWoL)
          tpsi <- psi_func(2, ZToL)
          qpsi <- psi_func(2, ZQoL)
          
          ZoW <- 0.011 * Wstar^2 / g + 0.11 * vis_air / Wstar
          denomW <- log(Zw / ZoW) - wpsi
          if (denomW < 0) {
            # denomW <- 10
          }
          denomW <- max(denomW, 1e-6, na.rm = TRUE)  # prevent div by zero)
          Wstar_new <- delw * kappa / denomW
          
          Wstar <- min(max(Wstar_new, 0.01), 5.0)
          
          # Wstar <- delw * kappa / denomW
          
          rr <- ZoW * Wstar / vis_air
          if (rr >= 0 && rr < 1000) {
            if (rr < 0.11)      idx <- 1
            else if (rr < 0.825) idx <- 2
            else if (rr < 3.0)   idx <- 3
            else if (rr < 10.0)  idx <- 4
            else if (rr < 30.0)  idx <- 5
            else if (rr < 100.0) idx <- 6
            else if (rr < 300.0) idx <- 7
            else if (rr < 1000.0) idx <- 8
            else {
              ier <- -2
              break
            }
            rt <- Liu_a[idx, 1] * rr^Liu_b[idx, 1]
            rq <- Liu_a[idx, 2] * rr^Liu_b[idx, 2]
          }
          
          # update Tstar, Qstar
          # print(Wstar)
          cff <- vis_air / Wstar
          # print(paste0(rt, ", ", cff))
          ZoT <- rt * cff
          ZoQ <- rq * cff
          cff <- kappa
          Tstar <- delt * cff / (log(zt / ZoT) - tpsi)
          Qstar <- delq * cff / (log(zq / ZoQ) - qpsi)
          
          TVstar <- Tstar * (1 + 0.61 * qa) + 0.61 * ta_k * Qstar
          Bf <- -g / ta_k * Wstar * TVstar
          if (Bf > 0) {
            beta <- 1.2
            r3 <- 1/3
            wgus <- beta * (Bf * Zabl)^r3
          } else {
            wgus <- 0
          }
          delw <- sqrt(w^2 + wgus^2)
        }
      }
      
      # compute bulk fluxes
      Wspeed <- sqrt(w^2 + wgus^2)
      Cd <- Wstar^2 / Wspeed^2
      qh <- cpa * rhoa * Wstar * Tstar
      
      if (rain_impact) {
        rainfall <- precip * 1000
        x1 <- 2.11e-5 * (ta_k / kelvin)^1.94
        x2 <- 0.02411 * (1 + ta * (3.309e-3 - 1.44e-6 * ta)) / (rhoa * cpa)
        x3 <- qa * L / (rgas * ta_k^2)
        cd_rain <- 1 / (1 + const06 * (x3 * L * x1) / (cpa * x2))
        cd_rain <- cd_rain * cpw * ((tw - ta) + (qs - qa) * L / cpa)
        qe <- qe - rainfall * cd_rain
      }
      
      qe <- L * rhoa * Wstar * Qstar
      
      # Webb correction
      upvel <- -1.61 * Wstar * Qstar - (1 + 1.61 * qa) * Wstar * Tstar / ta_k
      qe <- qe - rhoa * L * upvel * qa
      
      if (rain_impact && calc_evaporation) {
        evap <- rhoa / 1025 * Wstar * Qstar
      }
      
      taux <- rhoa * Cd * Wspeed * u10
      tauy <- rhoa * Cd * Wspeed * v10
      if (rain_impact) {
        tmp <- 0.85 * rainfall
        taux <- taux + tmp * u10
        tauy <- tauy + tmp * v10
      }
    }
  }
  
  return(list(taux = taux, tauy = tauy, qe = qe, qh = qh, evap = evap))
}

#' Stability function psi (translated from FORTRAN)
#' @param iflag 1 for wind, 2 for temp/humidity
#' @param ZoL Monin-Obukhov stability parameter
#' @returns psi value
#' @noRd
psi_func <- function(iflag, ZoL) {
  psi <- 0
  sqr3 <- sqrt(3)
  # unstable
  if (ZoL < 0) {
    chik <- (1 - 16 * ZoL)^(1/4)
    if (iflag == 1) {
      psik <- 2 * log(0.5 * (1 + chik)) +
        log(0.5 * (1 + chik^2)) -
        2 * atan(chik) + 0.5 * pi
    } else if (iflag == 2) {
      psik <- 2 * log(0.5 * (1 + chik^2))
    }
    chic <- (1 - 12.87 * ZoL)^(1/3)
    psic <- 1.5 * log((1/3) * (1 + chic + chic^3)) -
      sqr3 * atan((1 + 2 * chic) / sqr3) + pi / sqr3
    Fw <- 1 / (1 + ZoL^2)
    psi <- Fw * psik + (1 - Fw) * psic
  } else if (ZoL > 0) {
    # stable
    psi <- -4.7 * ZoL
  }
  psi
}

