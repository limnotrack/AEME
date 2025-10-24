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
      evap <- calc_fairall_vec(u10 = met[["u10"]], v10 = met[["v10"]],
                               sst = met[["sst"]], airt = met[["airt"]],
                               hum = met[["hum"]], airp = met[["airp"]],
                               precip = met[["precip"]])["evap"] |> 
        unlist()
    } else {
      evap <- sapply(seq_len(nrow(met)), \(n) {
        calc_kondo(u10 = met[["u10"]][n], v10 = met[["v10"]][n],
                   sst = met[["sst"]][n], airt = met[["airt"]][n],
                   hum = met[["hum"]][n], airp = met[["airp"]][n],
                   precip = met[["precip"]][n])
      })
    }
  } else if(model == "glm_aed") {
    
    # Source: https://github.com/AquaticEcoDynamics/GLM/blob/d18630994ef935fac8d9405ff0018b26c83ce271/src/glm_surface.c
    # Constants
    mwrw2a <- 18.016 / 28.966
    CE <- 0.0013
    
    # GLM variable names
    AirTemp <- met[["airt"]]
    LakeTemp <- met[["sst"]]
    Density <- rLakeAnalyzer::water.density(LakeTemp)
    RelHum <- met[["hum"]]
    AirPres <- met[["airp"]] /100 # Double check
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
    
  } else {
    Ts <- wtemp
    #saturation vapor pressure
    es <- exp(2.3026 *(((7.5*Ts)/(Ts+237.3)+0.7858)))
    #evaporative heat flux
    Qlh <- -((0.622/981.9) *         #constant/mean station pressure
               0.0013 *               #latent heat transfer coefficient
               1.168 *                #density of air
               2453000 *              #latent heat of evaporation of water
               wndspd *           #wind speed in m/s
               (prvapr - es))
  }
  return(evap)
}

#' Calculate atmospheric density
#'
#' @param atmosPressure [Pa]
#' @param vapPressure [Pa]
#' @param AirTemp [Cel]
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
