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

calc_fairall_vec <- function(sst, airt, u10, v10, airp, hum, precip,
                             rain_impact = TRUE, calc_evaporation = TRUE,
                             itermax = 40, tol = 1e-7, eps = 1e-12) {
  
  # --- constants ---
  kappa <- 0.41
  cpa <- 1008
  cpw <- 3985
  rho_0 <- 1025
  const06 <- 0.62198
  rgas <- 287.1
  g <- 9.81
  kelvin <- 273.15
  
  # Liu tables (unchanged)
  Liu_a <- matrix(c(0.177, 1.376, 1.026, 1.625, 4.661, 34.904, 1667.190, 588000.0,
                    0.292, 1.808, 1.393, 1.956, 4.994, 30.709, 1448.680, 298000.0),
                  ncol = 2, byrow = TRUE)
  Liu_b <- matrix(c(0.0, 0.929, -0.599, -1.018, -1.475, -2.067, -2.907, -3.935,
                    0.0, 0.826, -0.528, -0.870, -1.297, -1.845, -2.682, -3.616),
                  ncol = 2, byrow = TRUE)
  Liu_Rr <- c(0.0, 0.11, 0.825, 3.0, 10.0, 30.0, 100.0, 300.0, 1000.0)
  
  # empirical coefficients for saturation formula (same as original)
  a1 <- 6.107799961
  a2 <- 4.436518521e-1
  a3 <- 1.428945805e-2
  a4 <- 2.650648471e-4
  a5 <- 3.031240396e-6
  a6 <- 2.034080948e-8
  a7 <- 6.136820929e-11
  
  # other empirical params
  fdg <- 1.0
  beta <- 1.2
  Zabl <- 600.0
  zt <- 2.0
  zq <- 2.0
  zw <- 10.0
  wgust <- 0.0
  
  # --- input length handling (recycling like base R) ---
  n <- max(length(sst), length(airt), length(u10), length(v10),
           length(airp), length(hum), length(precip))
  sst <- rep(sst, length.out = n)
  airt <- rep(airt, length.out = n)
  u10 <- rep(u10, length.out = n)
  v10 <- rep(v10, length.out = n)
  airp <- rep(airp, length.out = n)
  hum <- rep(hum, length.out = n)
  precip <- rep(precip, length.out = n)
  
  # --- sanity / unit checks (basic heuristics) ---
  if (any(airp < 2000)) {
    # common mistake: air pressure in hPa (mb). Convert to Pa
    warning("Some airp < 2000 detected: treating airp as hPa (mb) and converting to Pa.")
    airp <- airp * 100
  }
  if (any(hum > 100 | hum < 0)) {
    warning("Relative humidity 'hum' contains values outside 0-100%. Check units.")
  }
  if (any(abs(sst) > 1000)) {
    warning("Some sst values are extremely large; check units.")
  }
  
  # psi function vectorised and numerically guarded
  psi_fun <- function(iflag, ZoL) {
    # ZoL can be vector
    res <- numeric(length(ZoL))
    small <- 1e-12
    ZoL_adj <- ZoL
    ZoL_adj[abs(ZoL_adj) < small] <- sign(ZoL_adj[abs(ZoL_adj) < small]) * small
    # unstable ZoL < 0
    idx_u <- ZoL_adj < 0
    if (any(idx_u)) {
      z <- ZoL_adj[idx_u]
      chik <- (1 - 16 * z) ^ 0.25
      psik <- 2 * log(0.5 * (1 + chik)) + log(0.5 * (1 + chik^2)) - 2 * atan(chik) + 0.5 * pi
      psic <- {
        chic <- (1 - 12.87 * z) ^ (1/3)
        1.5 * log((1/3) * (1 + chic + chic^2)) - sqrt(3) * atan((1 + 2 * chic) / sqrt(3)) + pi / sqrt(3)
      }
      Fw <- 1 / (1 + z^2)
      if (iflag == 1) {
        res[idx_u] <- Fw * psik + (1 - Fw) * psic
      } else if (iflag == 2) {
        res[idx_u] <- 2 * log(0.5 * (1 + chik^2)) * Fw + (1 - Fw) * psic  # keep weighting consistent
      }
    }
    # stable ZoL > 0
    idx_s <- ZoL_adj > 0
    if (any(idx_s)) {
      res[idx_s] <- -4.7 * ZoL_adj[idx_s]
    }
    return(res)
  }
  
  # helper to find Liu coefficients robustly
  get_rt_rq <- function(rr) {
    # rr scalar
    # if rr < first boundary -> use first
    k <- findInterval(rr, Liu_Rr, rightmost.closed = TRUE)
    # findInterval returns 0..length(Liu_Rr); we want 1..8 mapping
    k[k <= 0] <- 1
    k[k > length(Liu_Rr) - 1] <- length(Liu_Rr) - 1
    rt <- Liu_a[k, 1] * rr^Liu_b[k, 1]
    rq <- Liu_a[k, 2] * rr^Liu_b[k, 2]
    return(list(rt = rt, rq = rq))
  }
  
  # --- main loop over elements (vectorised via sapply/mapply) ---
  out <- lapply(seq_len(n), function(i) {
    # local copies
    ta_in <- airt[i]
    tw_in <- sst[i]
    u10_i <- u10[i]
    v10_i <- v10[i]
    p_i <- airp[i]
    hum_i <- hum[i]
    pr_i <- precip[i]
    
    # RH as fraction
    rh <- 0.01 * hum_i
    
    # convert temps: assume inputs are in degC unless > 100 => Kelvin
    if (ta_in < 100) {
      ta <- ta_in
      ta_k <- ta + kelvin
    } else {
      # assume Kelvin input
      ta_k <- ta_in
      ta <- ta_in - kelvin
    }
    if (tw_in < 100) {
      tw <- tw_in
      tw_k <- tw + kelvin
    } else {
      tw_k <- tw_in
      tw <- tw_in - kelvin
    }
    
    # saturation vapor pressure polynomial (original coefficients are in mb-ish form)
    ea_pol <- function(T) {
      a1 + T * (a2 + T * (a3 + T * (a4 + T * (a5 + T * (a6 + T * a7)))))
    }
    
    ea <- ea_pol(ta) * 100.0   # --> Pa
    ea <- ea * rh              # actual vapor pressure
    qa <- const06 * ea / (p_i - 0.377 * ea + eps)  # specific humidity (avoid denom=0)
    
    es <- ea_pol(tw) * 100.0   # saturation vapor pressure at SST
    es <- 0.98 * es            # seawater correction (Kraus 1972)
    qs <- const06 * es / (p_i - 0.377 * es + eps)
    
    rhoa <- p_i / (rgas * ta_k * (1 + const06 * qa) + eps)
    
    # initial winds and bulk quantities
    w <- sqrt(u10_i^2 + v10_i^2)
    delw <- sqrt(w^2 + wgust^2)
    
    # default outputs
    taux <- 0.0; tauy <- 0.0; qe <- 0.0; qh <- 0.0; evap <- 0.0
    
    if (delw > eps) {
      # viscosity of air (empirical)
      vis_air <- 1.326e-5 * (1 + ta * (6.542e-3 + ta * (8.301e-6 - 4.84e-9 * ta)))
      L <- (2.501 - 0.00237 * tw) * 1e6
      
      delq <- qa - qs
      delt <- ta - tw
      
      # initial guesses
      ZWoL <- 0.0
      ZoW <- 0.0005
      Wstar <- 0.04 * delw + eps
      Tstar <- 0.04 * delt
      Qstar <- 0.04 * delq
      TVstar <- Tstar * (1 + 0.61 * qa) + 0.61 * ta_k * Qstar
      
      ri <- g * zw * (delt + 0.61 * ta_k * delq) / (ta_k * delw^2 + eps)
      
      if (ri <= 0.25) {
        Wstar_old <- Wstar
        ier <- 0
        for (iter in seq_len(itermax)) {
          if (Wstar < eps) break
          oL <- g * kappa * TVstar / (ta_k * (1 + 0.61 * qa) * (Wstar^2 + eps))
          ZWoL <- zw * oL
          ZToL <- zt * oL
          ZQoL <- zq * oL
          
          wpsi <- psi_fun(1, ZWoL)
          tpsi <- psi_fun(2, ZToL)
          qpsi <- psi_fun(2, ZQoL)
          
          ZoW <- 0.011 * Wstar^2 / g + 0.11 * vis_air / (Wstar + eps)
          denomW <- (log(zw / (ZoW + eps)) - wpsi)
          if (abs(denomW) < eps) { ier <- -1; break }
          Wstar <- delw * kappa / denomW
          
          if (!is.finite(Wstar) || Wstar <= 0) { ier <- -2; break }
          
          rr <- ZoW * Wstar / (vis_air + eps)
          # Bound rr to positive range
          rr <- pmax(rr, 0)
          
          # obtain rt,rq
          lr <- get_rt_rq(rr)
          rt <- lr$rt; rq <- lr$rq
          
          cff <- vis_air / (Wstar + eps)
          ZoT <- rt * cff
          ZoQ <- rq * cff
          
          denomT <- log(zt / (ZoT + eps)) - tpsi
          denomQ <- log(zq / (ZoQ + eps)) - qpsi
          if (abs(denomT) < eps || abs(denomQ) < eps) { ier <- -3; break }
          
          cff2 <- kappa * fdg
          Tstar <- delt * cff2 / denomT
          Qstar <- delq * cff2 / denomQ
          
          bf <- -g / ta_k * Wstar * TVstar
          wgus <- if (bf > 0) beta * (bf * Zabl + eps)^(1/3) else 0
          delw <- sqrt(w^2 + wgus^2)
          
          # convergence
          if (abs(Wstar - Wstar_old) < tol) { ier <- 1; break }
          Wstar_old <- Wstar
        } # end iter
        
        if (ier >= 0) {
          Wspeed <- sqrt(w^2 + wgus^2)
          Cd <- Wstar^2 / (Wspeed^2 + eps)
          
          qh <- cpa * rhoa * Wstar * Tstar
          
          # bulk latent heat flux (qe) including rainfall effects
          qe_core <- L * rhoa * Wstar * Qstar
          qe <- qe_core
          
          if (rain_impact && pr_i > 0) {
            # rainfall (assume pr_i in m s^-1). Convert if user provided mm/hr externally.
            rainfall <- pr_i  # user should provide in m/s; no automatic mm->m conversion
            # empirical factors following original algorithm; keep denominators safe
            x1 <- 2.11e-5 * (ta_k / kelvin)^1.94
            x2 <- 0.02411 * (1 + ta * (3.309e-3 - 1.44e-6 * ta)) / (rhoa * cpa + eps)
            x3 <- qa * L / (rgas * ta_k^2 + eps)
            cd_rain <- 1 / (1 + const06 * (x3 * L * x1) / (cpa * x2 + eps) + eps)
            # the Fortran code then multiplies by cpw*(tw-ta) etc. Keep same structure:
            cd_rain <- cd_rain * cpw * ((tw - ta) + (qs - qa) * L / (cpa + eps))
            # subtract rain-induced flux (note: original used rainfall*1000; we don't multiply unless user supplied mm units)
            # If user has precip in m/s, a conversion to mass flux would require rho_water (~1000), but keep original functional structure.
            qe <- qe - rainfall * cd_rain
          }
          
          # additional upwelling / vertical velocity term
          upvel <- -1.61 * Wstar * Qstar - (1 + 1.61 * qa) * Wstar * Tstar / (ta_k + eps)
          qe <- qe - rhoa * L * upvel * qa
          
          if (rain_impact) {
            # momentum from wind
            cff <- rhoa * Cd * Wspeed
            taux <- cff * u10_i
            tauy <- cff * v10_i
            # simple momentum from rain term: original used tmp <- 0.85 * precip
            # That line is unit-sensitive; here we keep the same numeric factor but multiply by rho_water
            # If precip in m/s, momentum flux ~ rho_w * precip * U
            rho_w <- 1000
            tmp <- 0.85 * pr_i * rho_w
            taux <- taux + tmp * u10_i
            tauy <- tauy + tmp * v10_i
          } else {
            cff <- rhoa * Cd * Wspeed
            taux <- cff * u10_i
            tauy <- cff * v10_i
          }
          
          if (rain_impact && calc_evaporation) {
            # evap in m s^-1 (scaled by rhoa/rho0)
            evap <- rhoa / rho_0 * Wstar * Qstar
          }
        } # end ier >= 0
      } # end ri check
    } # end delw > eps
    
    return(c(taux = as.numeric(taux),
             tauy = as.numeric(tauy),
             qe = as.numeric(qe),
             qh = as.numeric(qh),
             evap = as.numeric(evap)))
  }) # end lapply
  
  # bind into data.frame
  mat <- do.call(rbind, out)
  res <- as.data.frame(mat)
  # ensure numeric columns
  res[] <- lapply(res, as.numeric)
  return(res)
}



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
        # print(iter)
        if (ier >= 0) {
          if (Wstar < 1e-200) break
          # Wstar <- ifelse(Wstar < 1e-200, 1e-100, Wstar)
          # oL <- g * kappa * TVstar / (ta_k * (1.0 + 0.61 * qa) * Wstar^2)
          oL <- g * kappa * TVstar / (ta_k * (1.0 + 0.61 * qa) * (Wstar^2 + eps))
          
          ZWoL <- zw * oL
          ZToL <- zt * oL
          ZQoL <- zq * oL

          wpsi <- psi(1, ZWoL)
          tpsi <- psi(2, ZToL)
          qpsi <- psi(2, ZQoL)

          ZoW <- 0.011 * Wstar^2 / g + 0.11 * vis_air / Wstar
          Wstar <- delw * kappa / (log(zw / ZoW + eps) - wpsi)
          # Wstar <- delw * kappa / (log(zw / ZoW) - wpsi)

          rr <- ZoW * Wstar / vis_air
          # cat("rr: ", rr, "\n", "ZoW: ", ZoW, "\n", "Wstar: ", Wstar, "\n", "vis_air: ", vis_air, "\n", "iter: ", iter, "\n")
          if (rr >= 0.0 & rr < 1000.0) {
            for (k in 1:8) {
              if (Liu_Rr[k] <= rr && rr < Liu_Rr[k + 1]) {
                rt <- Liu_a[k, 1] * rr^Liu_b[k, 1]
                rq <- Liu_a[k, 2] * rr^Liu_b[k, 2]
              }
              if (rr >= Liu_Rr[8]) {
                rt <- Liu_a[8, 1] * rr^Liu_b[8, 1]
                rq <- Liu_a[8, 2] * rr^Liu_b[8, 2]
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
        if (iter > 1 && abs(Wstar - Wstar_old) < 1e-6) break
        Wstar_old <- Wstar
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

# Function to compute stability function psi
psi <- function(iflag, ZoL) {
  r3 <- 1.0/3.0
  sqr3 <- sqrt(3)
  pi <- pi
  if (abs(ZoL) < 1e-6) ZoL <- sign(ZoL) * 1e-6
  
  # Initialize for the zero "ZoL" case.
  psi <- 0.0

  # Unstable conditions.
  if (ZoL < 0.0) {
    chik <- (1.0 - 16.0 * ZoL) ^ 0.25
    if (iflag == 1) {
      psik <- 2.0 * log(0.5 * (1.0 + chik)) + log(0.5 * (1.0 + chik^2)) -
        2.0 * atan(chik) + 0.5 * pi
    } else if (iflag == 2) {
      psik <- 2.0 * log(0.5 * (1.0 + chik^2))
    }

    # For very unstable conditions, use free-convection (Fairall).
    chic <- (1.0 - 12.87 * ZoL) ^ r3
    psic <- 1.5 * log(r3 * (1.0 + chic + chic^2)) - sqr3 * atan((1.0 + 2.0 * chic) / sqr3) + pi / sqr3

    # Match Kansas and free-convection forms with weighting Fw.
    Fw <- 1.0 / (1.0 + ZoL^2)
    psi <- Fw * psik + (1.0 - Fw) * psic

  } else if (ZoL > 0.0) {
    # Stable conditions.
    psi <- -4.7 * ZoL
  }

  return(psi)
}
