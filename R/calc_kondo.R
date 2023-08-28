calc_kondo <- function(sst, airt, u10, v10, precip, hum, airp, rain_impact = TRUE, 
                       calc_evaporation = TRUE) {
  
  kelvin = 273.15
  eps=1.0e-12
  cpa=1008
  cpw=3985
  emiss=0.97
  bolz=5.67e-8

  rgas = 287.1
  g = 9.81
  rho_0 = 1025
  kappa = 0.41
  const06 <- 0.62198
  
  a1 <- 6.107799961
  a2 <- 4.436518521e-1
  a3 <- 1.428945805e-2
  a4 <- 2.650648471e-4
  a5 <- 3.031240396e-6
  a6 <- 2.034080948e-8
  a7 <- 6.136820929e-11  
  
  
  ae_d=c( 0., 0.771, 0.867, 1.2  , 0.    )
  ae_h=c( 0., 0.927, 1.15 , 1.17 , 1.652 )
  ae_e=c( 0., 0.969, 1.18 , 1.196, 1.68  )
  be_d=c( 1.08 , 0.0858, 0.0667, 0.025 ,  0.073 )
  be_h=c( 1.185, 0.0546, 0.01  , 0.0075, -0.017 )
  be_e=c( 1.23 , 0.0521, 0.01  , 0.008 , -0.016 )
  ce_h=c( 0., 0., 0., -0.00045, 0. )
  ce_e=c( 0., 0., 0., -0.0004 , 0. )
  pe_d=c( -0.15 , 1., 1., 1., 1. )
  pe_h=c( -0.157, 1., 1., 1., 1. )
  pe_e=c( -0.16 , 1., 1., 1., 1. )
  
  
  w = sqrt(u10*u10+v10*v10)
  L = (2.5-0.00234*sst)*1.e6
  
  tw  = sst
  tw_k= sst+kelvin

  ta_k  = airt + kelvin
  ta_K  = airt + kelvin
  ta = airt
  
  rh <- 0.01 * hum # if hum is in %
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
  
  s0=0.25*(sst-airt)/(w+1.0e-10)^2
  s=s0*abs(s0)/(abs(s0)+0.01)
  
  if (w < 2.2) {
    x = log(w+eps)
    cdd=(be_d[1]*exp(pe_d[1]*x))*1.0e-3
    chd=(be_h[1]*exp(pe_h[1]*x))*1.0e-3
    ced=(be_e[1]*exp(pe_e[1]*x))*1.0e-3
  } else if (w < 5.0) {
    x = exp(log(w+eps))
    cdd=(ae_d[2]+be_d[2]*x)*1.0e-3
    chd=(ae_h[2]+be_h[2]*x)*1.0e-3
    ced=(ae_e[2]+be_e[2]*x)*1.0e-3
  } else if (w < 8.0) {
    x = exp(log(w+eps))
    cdd=(ae_d[3]+be_d[3]*x)*1.0e-3
    chd=(ae_h[3]+be_h[3]*x)*1.0e-3
    ced=(ae_e[3]+be_e[3]*x)*1.0e-3
  } else if (w < 25.0) {
    x = exp(log(w+eps))
    cdd=(ae_d[4]+be_d[4]*x)*1.0e-3
    chd=(ae_h[4]+be_h[4]*x+ce_h[4]*(w-8.0)^2)*1.0e-3
    ced=(ae_e[4]+be_e[4]*x+ce_e[4]*(w-8.0)^2)*1.0e-3
  } else {
    x = exp(log(w+eps))
    cdd=(ae_d(5)+be_d(5)*x)*1.0e-3
    chd=(ae_h(5)+be_h(5)*x)*1.0e-3
    ced=(ae_e(5)+be_e(5)*x)*1.0e-3
  }
  
  if(s < 0.) {
    if (s > -3.3) {
      x = 0.1+0.03*s+0.9*exp(4.8*s)
    } else {
      x = 0.0
    }
    cdd=x*cdd
    chd=x*chd
    ced=x*ced
  } else {
    cdd=cdd*(1.0+0.47*sqrt(s))
    chd=chd*(1.0+0.63*sqrt(s))
    ced=ced*(1.0+0.63*sqrt(s))
  }
  
  qh=-chd*cpa*rhoa*w*(sst-airt)       # sensible
  qe=-ced*L*rhoa*w*(qs-qa)            # latent
  
  if (rain_impact) {
    rainfall=precip * 1000. # (convert from m/s to kg/m2/s)
    x1 = 2.11e-5*(ta_k/kelvin)**1.94
    x2 = 0.02411*(1.0+ta*(3.309e-3-1.44e-6*ta))/(rhoa*cpa)
    x3 = qa * L /(rgas * ta_K * ta_K)
    cd_rain = 1.0/(1.0+const06*(x3*L*x1)/(cpa*x2))
    cd_rain = cd_rain*cpw*((tw-ta) + (qs-qa)*L/cpa)
    qe = qe - rainfall * cd_rain
  }
  #     units of qs and qa - should be kg/kg

  #  calculation of evaporation/condensation in m/s
  if (rain_impact & calc_evaporation) {
    #     ced from latent heatflux for moisture flux
    evap = rhoa/rho_0*ced*w*(qa-qs)
  } else {
    evap = 0
  }
  return(evap)
  # 
  # tmp = cdd*rhoa*w
  # taux = tmp*u10
  # tauy = tmp*v10
  # 
  # !  Compute momentum flux (N/m2) due to rainfall (kg/m2/s).
  # !  according to Caldwell and Elliott (1971, JPO)
  # if ( rain_impact ) then
  # tmp  = 0.85d0 * rainfall
  # taux  = taux + tmp * u10
  # tauy  = tauy + tmp * v10
  # end if
}
