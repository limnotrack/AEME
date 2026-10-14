#
# wind_speed <- sub$met$MET_wndspd
# usquared = 1.612e-6 * wind_speed * wind_speed
# # XMoment1 = calc_xmoment(NLayers, iheight, density)
# U_sensM = wind_speed
# WIND_HEIGHT=10
# c_z0=0.0001
# U10 = wind_speed * (log(10.0/c_z0)/log(WIND_HEIGHT/c_z0))
# head(U10)
# head(wind_speed)
#
# CDN10 = 1.92E-7 * U10*U10*U10 + 0.00096
# CDN10[CDN10>0.0025] <- 0.0025
#
# Ux = sqrt(CDN10  * U_sensM * U_sensM)
# z0 = (0.012*Ux*Ux/g) + 0.11*visc_k_air/Ux
# CDN10 = pow(vonK/log(10./z0),2.0)

calc_evap <- function(met, altitude,
                      model = "gotm_wet", method = "fairall", gusty = FALSE) {
  
  if (model == "gotm_wet") {
    if (method == "fairall") {
      humid <- calc_humidity_vars(hum_method = 1, hum = met[["hum"]], 
                                  tw = met[["sst"]], ta = met[["airt"]], airp = met[["airp"]])
      evap <- calc_fairall_vec(sst = met[["sst"]], airt = met[["airt"]],
                               u10 = met[["u10"]], v10 = met[["v10"]],
                               precip = met[["precip"]], qa = humid[["qa"]], 
                               qs = humid[["qs"]], rhoa = humid[["rhoa"]])["evap"] |>
        unlist()
      # evap <- calc_fairall_vec(u10 = met[["u10"]], v10 = met[["v10"]],
      #                          sst = met[["sst"]], airt = met[["airt"]],
      #                          hum = met[["hum"]], airp = met[["airp"]],
      #                          precip = met[["precip"]])["evap"] |> 
      #   unlist()
    } else {
      evap <- sapply(seq_len(nrow(met)), \(n) {
        calc_kondo(u10 = met[["u10"]][n], v10 = met[["v10"]][n],
                   sst = met[["sst"]][n], airt = met[["airt"]][n],
                   hum = met[["hum"]][n], airp = met[["airp"]][n],
                   precip = met[["precip"]][n])
      })
    }
  } else if (model == "glm_aed") {

    # Source: https://github.com/AquaticEcoDynamics/GLM/blob/d18630994ef935fac8d9405ff0018b26c83ce271/src/glm_surface.c
    # Constants
    mwrw2a <- 18.016 / 28.966
    CE <- 0.0013

    # GLM variable names
    AirTemp <- met[["airt"]]
    LakeTemp <- met[["sst"]]
    Density <- rLakeAnalyzer::water.density(LakeTemp)
    RelHum <- met[["hum"]]
    AirPres <- met[["airp"]] / 100 # Double check
    WindSp <- sqrt(met[["u10"]]^2 + met[["v10"]]^2)
    SatVapDef <- (RelHum/100) * saturated_vapour(AirTemp)
    SatVap_surface <- saturated_vapour(LakeTemp) #hPa
    p_atm <- ((100*AirPres) * ((1 - 2.25577e-5*altitude) ^5.25588))/100
    latent_heat_vap <- 2.501e6 - 2370*LakeTemp

    rho_air <- atm_density(p_atm*100.0, SatVapDef*100.0, AirTemp) # kg/m3
    # rho_o <- atm_density(p_atm*100.0, SatVap_surface*100.0, LakeTemp) # kg/m3

    Q_latentheat <- -CE * rho_air * latent_heat_vap * (mwrw2a/p_atm) * WindSp * (SatVap_surface - SatVapDef)
    Q_latentheat[Q_latentheat > 0] <- 0 # no condensation
    # evap <- Q_latentheat / (latent_heat_vap)

    evap <- Q_latentheat / (latent_heat_vap * Density)

  } else if (model == "simstrat_aed2") {

    # Source: Simstrat's own strat_forcing.f90 (free-water, non-ice case).
    # A Livingstone & Imboden (1989)-style wind function: free convection
    # (driven by the water-air temperature difference, wind-independent)
    # and forced convection (wind speed) combined in quadrature, applied to
    # a Gill (1992) saturation vapour pressure - structurally different
    # from the GLM-style bulk-aerodynamic formula above (which is linear in
    # wind speed only). Verified against real met data: for this package's
    # test lake the two formulas correlate at ~0.99 (Simstrat's runs ~18%
    # lower), so in practice they behave similarly here, but this is the
    # formula Simstrat itself actually integrates, so it's the correct one
    # to use when fitting the water balance for simstrat_aed2.
    T_surf  <- met[["sst"]]
    T_atm   <- met[["airt"]]
    uv10    <- sqrt(met[["u10"]]^2 + met[["v10"]]^2)
    p_air   <- met[["airp"]] / 100 # Pa -> mbar
    Vap_atm <- if (!is.null(met[["vap"]])) {
      met[["vap"]] # actual vapour pressure (mbar), as fed to Simstrat directly
    } else {
      (met[["hum"]] / 100) * saturated_vapour_gill(T_atm, p_air) # fallback from RH
    }

    fu <- sqrt(
      (2.7 * pmax(0, (T_surf - T_atm) / (1 - 0.378 * Vap_atm / p_air))^0.333)^2 +
        (0.6072 * 3.1 * uv10)^2
    )

    Vap_wat <- saturated_vapour_gill(T_surf, p_air)
    H_V <- -fu * (Vap_wat - Vap_atm) # W/m2
    H_V[H_V > 0] <- 0 # no condensation

    latent_heat_vap <- 2.501e6 - 2370 * T_surf
    Density <- rLakeAnalyzer::water.density(T_surf)
    evap <- H_V / (latent_heat_vap * Density) # m/s

  } else {
    cli::cli_abort(
      "Unsupported {.arg model} value {.val {model}} -- must be one of
      {.val gotm_wet}, {.val glm_aed}, or {.val simstrat_aed2}.",
      class = "aeme_error_calc_evap_model"
    )
  }
  return(evap)
}

#' Calculate atmospheric density
#'
#' @param atmosPressure (Pa)
#' @param vapPressure (Pa)
#' @param AirTemp (Cel)
#'
#' @return vector of atmospheric density
#' @noRd
#'
atm_density <- function(atmosPressure, vapPressure, AirTemp) {
  Kelvin <- 273.15
  mwrw2a <- 18.016 / 28.966
  c_gas <- 1.0E3 * 8.31436 / 28.966
  r <- mwrw2a * vapPressure/(atmosPressure - vapPressure)
  return(1.0/c_gas * (1 + r)/(1 + r/mwrw2a) * atmosPressure/(AirTemp+Kelvin))
}

saturated_vapour <- function(AirTemp) {
  Kelvin <- 273.15
  (9.28603523 - (2322.37885/(AirTemp + Kelvin)))^10
}

#' Saturation vapour pressure (Gill 1992), as used by Simstrat
#'
#' @param Temp numeric; temperature (degC)
#' @param p_air numeric; air pressure (mbar)
#'
#' @return vector of saturation vapour pressure (mbar)
#' @noRd
saturated_vapour_gill <- function(Temp, p_air) {
  Vap <- 10^((0.7859 + 0.03477 * Temp) / (1 + 0.00412 * Temp))
  Vap * (1 + 1e-6 * p_air * (4.5 + 0.00006 * Temp^2))
}
