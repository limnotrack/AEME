#' @title Calculate Fairall et al 1996 bulk fluxes
#' @description Calculate airsea fluxes using Fairall et al 1996
#' @param sst Sea surface temperature [degC]
#' @param airt Air temperature [degC]
#' @param u10 Eastward wind speed at 10m [m/s]
#' @param v10 Northward wind speed at 10m [m/s]
#' @param airp Air pressure [Pa]
#' @param hum Relative humidity [%]
#' @param precip Precipitation rate [m/s]
#' @param rain_impact Logical, should rain impact be included? Default is TRUE
#' @param calc_evaporation Logical, should evaporation be calculated? Default is TRUE
#' @return List with the following elements:
#' \itemize{
#'  \item{tau_x}{Eastward wind stress [N/m^2]}
#'  \item{tau_y}{Northward wind stress [N/m^2]}
#'  \item{h}{Sensible heat flux [W/m^2]}
#'  \item{e}{Latent heat flux [W/m^2]}
#'  \item{evap}{Evaporation rate [m/s]}
#'  }
#'
#'  @noRd
#'

calc_fairall <- function(sst, airt, u10, v10, airp, hum, precip,
                          rain_impact = TRUE, calc_evaporation = TRUE) {
  # Constants
  fdg <- 1.0
  beta <- 1.2
  Zabl <- 600.0
  r3 <- 1.0/3.0
  kelvin <- 273.15
  Liu_a <- matrix(c(0.177, 1.376, 1.026, 1.625, 4.661, 34.904, 1667.190, 588000.0,
                    0.292, 1.808, 1.393, 1.956, 4.994, 30.709, 1448.680, 298000.0), ncol = 2, byrow = TRUE)
  Liu_b <- matrix(c(0.0, 0.929, -0.599, -1.018, -1.475, -2.067, -2.907, -3.935,
                    0.0, 0.826, -0.528, -0.870, -1.297, -1.845, -2.682, -3.616), ncol = 2, byrow = TRUE)
  Liu_Rr <- c(0.0, 0.11, 0.825, 3.0, 10.0, 30.0, 100.0, 300.0, 1000.0)
  zt <- 2.0
  zq <- 2.0
  zw <- 10.0
  itermax <- 20
  eps <- 1.0e-12
  wgust <- 0.0
  kappa = 0.41
  cpa=1008
  cpw=3985
  rho_0 = 1025

  a1 <- 6.107799961
  a2 <- 4.436518521e-1
  a3 <- 1.428945805e-2
  a4 <- 2.650648471e-4
  a5 <- 3.031240396e-6
  a6 <- 2.034080948e-8
  a7 <- 6.136820929e-11

  const06 <- 0.62198
  rgas = 287.1
  g = 9.81


  rh <- 0.01 * hum # if hum is in %
  ta <- airt
  tw <- sst


  # saturation vapor pressure at that air temperature
  ea <- a1 +ta*(a2+ta*(a3+ta*(a4+ta*(a5+ta*(a6+ta*a7)))))
  ea <- ea * 100.0 #  Conversion millibar --> Pascal
  # get actual vapor pressure
  ea <- rh * ea
  # convert to specific humidity
  qa <- const06*ea/(airp-0.377*ea)

  #  saturation vapor pressure - using SST
  es <- a1 +tw*(a2+tw*(a3+tw*(a4+tw*(a5+tw*(a6+tw*a7)))))
  es <- es * 100.0 #  Conversion millibar --> Pascal

  rhoa = airp/(rgas*(ta+kelvin)*(1.0+const06*qa))

  # correction for seawater, following Kraus 1972
  # correcting for salt water assuming 98% RH
  es <- 0.98 * es
  # saturation specific humidity
  qs <- const06*es/(airp-0.377*es)


  # Conversion from Fortran to R starts here
  evap <- 0.0
  w <- sqrt(u10^2 + v10^2)

  if (max(sst) < 100) {
    tw <- sst
    tw_k <- sst + kelvin
  } else {
    tw <- sst - kelvin
    tw_k <- sst
  }

  if (max(airt) < 100) {
    ta_k <- airt + kelvin
    ta <- airt
  } else {
    ta <- airt - kelvin
    ta_k <- airt
  }

  qe <- 0.0
  qh <- 0.0
  taux <- 0.0
  tauy <- 0.0
  delw <- sqrt(w^2 + wgust^2)

  if (delw != 0.0) {
    vis_air <- 1.326e-5 * (1.0 + ta * (6.542e-3 + ta * (8.301e-6 - 4.84e-9 * ta)))
    L <- (2.501 - 0.00237 * tw) * 1e6

    ier <- 0
    delq <- qa - qs
    delt <- ta - tw

    ZWoL <- 0.0
    ZoW <- 0.0005
    Wstar <- 0.04 * delw
    Tstar <- 0.04 * delt
    Qstar <- 0.04 * delq
    TVstar <- Tstar * (1.0 + 0.61 * qa) + 0.61 * ta_k * Qstar

    ri <- g * zw * (delt + 0.61 * ta_k * delq) / (ta_k * delw^2)

    if (ri <= 0.25) {
      for (iter in 1:itermax) {
        if (ier >= 0) {
          oL <- g * kappa * TVstar / (ta_k * (1.0 + 0.61 * qa) * Wstar^2)
          ZWoL <- zw * oL
          ZToL <- zt * oL
          ZQoL <- zq * oL

          wpsi <- psi(1, ZWoL)
          tpsi <- psi(2, ZToL)
          qpsi <- psi(2, ZQoL)

          ZoW <- 0.011 * Wstar^2 / g + 0.11 * vis_air / Wstar
          Wstar <- delw * kappa / (log(zw / ZoW) - wpsi)

          rr <- ZoW * Wstar / vis_air
          if (rr >= 0.0 & rr < 1000.0) {
            for (k in 1:8) {
              if (Liu_Rr[k] <= rr && rr < Liu_Rr[k + 1]) {
                rt <- Liu_a[k, 1] * rr^Liu_b[k, 1]
                rq <- Liu_a[k, 2] * rr^Liu_b[k, 2]
              }
            }

            cff <- vis_air / Wstar
            ZoT <- rt * cff
            ZoQ <- rq * cff
            cff <- kappa * fdg
            Tstar <- delt * cff / (log(zt / ZoT) - tpsi)
            Qstar <- delq * cff / (log(zq / ZoQ) - qpsi)

            bf <- -g / ta_k * Wstar * TVstar
            if (bf > 0) {
              wgus <- beta * (bf * Zabl)^r3
            } else {
              wgus <- 0.0
            }
            delw <- sqrt(w^2 + wgus^2)
          } else {
            ier <- -2
          }
        }
      }

      if (ier >= 0) {
        Wspeed <- sqrt(w^2 + wgus^2)
        Cd <- Wstar^2 / Wspeed^2

        qh <- cpa * rhoa * Wstar * Tstar
        if (rain_impact) {
          rainfall <- precip * 1000
          x1 <- 2.11e-5 * (ta_k / kelvin)^1.94
          x2 <- 0.02411 * (1.0 + ta * (3.309e-3 - 1.44e-6 * ta)) / (rhoa * cpa)
          x3 <- qa * L / (rgas * ta_k * ta_k)
          cd_rain <- 1.0 / (1.0 + const06 * (x3 * L * x1) / (cpa * x2))
          cd_rain <- cd_rain * cpw * ((tw - ta) + (qs - qa) * L / cpa)
          qe <- qe - rainfall * cd_rain
        }

        qe <- L * rhoa * Wstar * Qstar

        upvel <- -1.61 * Wstar * Qstar - (1.0 + 1.61 * qa) * Wstar * Tstar / ta_k
        qe <- qe - rhoa * L * upvel * qa

        if (rain_impact && calc_evaporation) {
          evap <- rhoa / rho_0 * Wstar * Qstar
        }

        cff <- rhoa * Cd * Wspeed
        taux <- cff * u10
        tauy <- cff * v10

        if (rain_impact) {
          tmp <- 0.85 * precip
          taux <- taux + tmp * u10
          tauy <- tauy + tmp * v10
        }
      }
    }
  }

  return(list(taux = taux, tauy = tauy, qe = qe, qh = qh, evap = evap))
}

psi <- function(iflag, ZoL) {
  r3 <- 1.0/3.0
  sqr3 <- sqrt(3)
  pi <- pi
  Fw <- 0.0

  psik <- function(chik) {
    if (iflag == 1) {
      return(2.0 * log(0.5 * (1.0 + chik)) + log(0.5 * (1.0 + chik^2)) - 2.0 * atan(chik) + 0.5 * pi)
    } else if (iflag == 2) {
      return(2.0 * log(0.5 * (1.0 + chik^2)))
    }
  }

  chic <- function(ZoL) {
    return((1.0 - 12.87 * ZoL)^(1.0/3.0))
  }

  chik <- chic(ZoL)
  psic <- 1.5 * log(r3 * (1.0 + chik + chik^2)) - sqr3 * atan((1.0 + 2.0 * chik) / sqr3) + pi / sqr3

  Fw <- 1.0 / (1.0 + ZoL^2)
  psi <- Fw * psik(chik) + (1.0 - Fw) * psic

  return(psi)
}

# nc_file <- list.files(path, recursive = T, pattern = "output.nc",
#                       full.names = T)
#
# nc <- ncdf4::nc_open(nc_file)
# airt <- ncdf4::ncvar_get(nc, "airt")
# airp <- ncdf4::ncvar_get(nc, "airp")
# hum <- ncdf4::ncvar_get(nc, "hum")
# precip <- ncdf4::ncvar_get(nc, "precip")
# sst <- ncdf4::ncvar_get(nc, "sst")
# u10 <- ncdf4::ncvar_get(nc, "u10")
# v10 <- ncdf4::ncvar_get(nc, "v10")
#
# evap <- ncdf4::ncvar_get(nc, "evap")
# qh <- ncdf4::ncvar_get(nc, "qh")
# qe <- ncdf4::ncvar_get(nc, "qe")
#
# ncdf4::nc_close(nc)
#
# i <- 6
#
# tst <- calc_fairall(sst = sst[i], airt = airt[i],
#                     u10 = u10[i],  v10 = v10[i],
#                     hum = hum[i],
#                     airp = airp[i],
#                     precip = precip[i],
#                     rain_impact = T,
#                     calc_evaporation = T)
#
# tst
# evap[i]
# qh[i]
# qe[i]
#
# calc_fairall(sst = met$sst[i], airt = met$airt[i],
#              u10 = met$u10[i],  v10 = met$v10[i],
#              hum = met$hum[i],
#              airp = met$airp[i],
#              precip = met$precip[i],
#              rain_impact = T,
#              calc_evaporation = T)

#
# calc_fairall2(sst = gotm_met$sst[1], airt = gotm_met$airt[1],
#              u10 = gotm_met$u10[1],  v10 = gotm_met$v10[1],
#              precip = gotm_met$precip[1], hum = gotm_met$hum[1],
#              airp = gotm_met$airp[1] * 100)
# calc_fairall(sst = gotm_met$sst[1], airt = gotm_met$airt[1],
#               u10 = gotm_met$u10[1],  v10 = gotm_met$v10[1],
#                hum = gotm_met$hum[1],
#               airp = gotm_met$airp[1] * 100)
