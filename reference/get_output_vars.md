# Get the output variables from an AEME object

Get the output variables from an AEME object

## Usage

``` r
get_output_vars(aeme, model, ens_n = 1)
```

## Arguments

- aeme:

  aeme; object.

- model:

  vector; of models to be used. Can be \`dy_cd\`, \`glm_aed\`,
  \`gotm_wet\`.

- ens_n:

  numeric; ensemble number to allocate to model output which is loaded.
  Defaults to 1.

## Value

A character vector of the output variables

## Examples

``` r
aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
aeme <- readRDS(aeme_file)
path <- tempdir()
model_controls <- get_model_controls(use_bgc = TRUE)
model <- c("glm_aed", "gotm_wet")
aeme <- build_aeme(path = path, aeme = aeme, model = model,
                   model_controls = model_controls,
                   ext_elev = 5, use_bgc = TRUE)
#> ! Missing state variables in inflows:
#> ! ZOO_zoo1
#> ℹ Added default values for missing variables.
#> ℹ Using observed water level
#> ! Missing values in observed water level
#> ℹ Insufficient water level observations. Using constant water level
#> ℹ Correcting water balance using estimated outflows (method = 2).
#> ℹ Calculating lake level using lake depth and a sinisoidal function.
#> ℹ Building GLM-AED2 for lake wainamu
#> ℹ Copied in GLM nml file
#> ℹ Copied in AED nml file and supporting files
#> ℹ 15 replaced with 41.6285
#> ℹ Using default pH initialisation
#> ℹ 15 replaced with 16.6514
#> ℹ 225 replaced with 312.5
#> ℹ 2.25 replaced with 1.4279
#> ℹ 21 replaced with 21.4183
#> ℹ 6.96 replaced with 1.0709
#> ℹ 19.8 replaced with 7.1394
#> ℹ 0.008 replaced with 0.3229
#> ℹ 0.05 replaced with 0.3229
#> ℹ 0.05 replaced with 0.3229
#> ℹ PHY_cyano 2 replaced with 0.24022
#> ℹ PHY_diatom 10 replaced with 0.300275
#> ℹ PHY_green 0.04 replaced with 0.300275
#> ℹ 100 replaced with 1
#> ℹ Using default zooplankton initialisation
#> ℹ Building GOTM-WET model for lake wainamu
#> ℹ Copied in GOTM configuration files
#> ℹ instances/abiotic_water/initialization/sDDOMW 2.5 replaced with 0.5
#> ℹ instances/abiotic_water/initialization/sDPOMW 0.1 replaced with 0.2
#> ℹ instances/abiotic_water/initialization/sO2W 13 replaced with 10
#> ℹ instances/abiotic_water/initialization/sDIMW 4 replaced with 3
#> ℹ instances/abiotic_water/initialization/sNH4W 0.05 replaced with 0.02
#> ℹ instances/abiotic_water/initialization/sNDOMW 0.01 replaced with 0.3
#> ℹ instances/abiotic_water/initialization/sNO3W 0.5 replaced with 0.015
#> ℹ instances/abiotic_water/initialization/sNPOMW 0.01 replaced with 0.1
#> ℹ instances/abiotic_water/initialization/sPDOMW 0.001 replaced with 0.01
#> ℹ instances/abiotic_water/initialization/sPO4W 0.1 replaced with 0.01
#> ℹ instances/abiotic_water/initialization/sPPOMW 0.001 replaced with 0.01
#> ℹ Setting initial condition for instances/cyanobacteria/initialization/sDW: 0.1
#>   replaced with 0.2
#> ℹ Setting initial condition for instances/cyanobacteria/initialization/sNW:
#>   0.03 replaced with 0.03
#> ℹ Setting initial condition for instances/cyanobacteria/initialization/sPW:
#>   0.003 replaced with 0.0019
#> ℹ Setting initial condition for instances/diatoms/initialization/sDW: 0.2
#>   replaced with 0.25
#> ℹ Setting initial condition for instances/diatoms/initialization/sNW: 0.05
#>   replaced with 0.038
#> ℹ Setting initial condition for instances/diatoms/initialization/sPW: 0.005
#>   replaced with 0.0024
#> ℹ Setting initial condition for instances/greens/initialization/sDW: 0.1
#>   replaced with 0.1
#> ℹ Setting initial condition for instances/greens/initialization/sNW: 0.05
#>   replaced with 0.015
#> ℹ Setting initial condition for instances/greens/initialization/sPW: 0.001
#>   replaced with 0.00094
#> ℹ instances/abiotic_water/initialization/sSiO2W 3.5 replaced with 1
#> ℹ instances/cladocerans/initialization/sD 0.05 replaced with 1
#> ℹ Setting initial condition forinstances/cladocerans/initialization/sN: 0.0035
#>   replaced with 0.07
#> ℹ Setting initial condition forinstances/cladocerans/initialization/sP: 5e-04
#>   replaced with 0.01
#> ✔ GOTM YAML validation completed - no issues detected.
#> ✔ GLM nml validation completed - no issues detected.
# Run models
aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
path = path, model_controls = model_controls,
parallel = TRUE, ncores = 2L)
#> ℹ Running models in parallel... [2025-12-17 22:58:42]
#> ✔ Model run complete! [2025-12-17 22:58:46]
#> ℹ Reading models in parallel...[2025-12-17 22:58:46]
#> ✔ Model reading complete! [2025-12-17 22:58:49]
get_output_vars(aeme, model)
#>                    Water temperature                    Thermocline depth 
#>                           "HYD_temp"                         "HYD_thmcln" 
#>                     Dissolved oxygen                  Total chlorophyll a 
#>                            "CHM_oxy"                          "PHY_tchla" 
#>                       Total nitrogen                     Total phosphorus 
#>                             "NIT_tn"                             "PHS_tp" 
#>                Evaporative heat flux                   Sensible heat flux 
#>                             "LKE_Qe"                             "LKE_Qh" 
#>                   Longwave radiation                  Shortwave radiation 
#>                            "LKE_Qlw"                            "LKE_Qsw" 
#>                               Volume                          Evaporation 
#>                              "LKE_V"                         "LKE_evpvol" 
#>                          Evaporation                         Surface area 
#>                         "LKE_evpflx"                             "LKE_A0" 
#>                          Evaporation                               Inflow 
#>                         "LKE_evprte"                         "LKE_inflow" 
#>                              Outflow                        Precipitation 
#>                        "LKE_outflow"                         "LKE_precip" 
#>                        Precipitation       Remote sensed skin temperature 
#>                         "LKE_pcpvol"                          "HYD_surft" 
#>                      e-folding depth                       Euphotic depth 
#>                          "LKE_efold"                         "LKE_photic" 
#>                                 Time                           Lake depth 
#>                               "Date"                         "LKE_depths" 
#>                        Water density                             Salinity 
#>                           "HYD_dens"                           "CHM_salt" 
#>                            Phosphate                  Dissolved organic P 
#>                            "PHS_frp"                            "PHS_dop" 
#>                Particulate organic P                  Ammoniacal nitrogen 
#>                            "PHS_pop"                            "NIT_amm" 
#>                              Nitrate                  Dissolved organic N 
#>                            "NIT_nit"                            "NIT_don" 
#>                Particulate organic N             Dissolved organic carbon 
#>                            "NIT_pon"                            "CAR_doc" 
#>           Particulate organic carbon                        Cyanobacteria 
#>                            "CAR_poc"                          "PHY_cyano" 
#>                          Green algae                   Diatoms freshwater 
#>                          "PHY_green"                         "PHY_diatom" 
#>                           Stratified                    Schmidt stability 
#>                          "HYD_strat"                         "HYD_schstb" 
#>                   Centre of buoyancy                     Epilimnion depth 
#>                         "HYD_ctrbuy"                         "HYD_epidep" 
#>                    Hypolimnion depth    Trophic Level Index Chlorophyll-a 
#>                         "HYD_hypdep"                           "LKE_tlic" 
#>   Trophic Level Index Total Nitrogen Trophic Level Index Total Phosphorus 
#>                           "LKE_tlin"                           "LKE_tlip" 
#>     Trophic Level Index Secchi depth                Trophic Level Index 3 
#>                          "LKE_tlise"                           "LKE_tli3" 
#>                Trophic Level Index 4                       Oxycline depth 
#>                           "LKE_tli4"                         "CHM_oxycln" 
#>                   Epilimnetic oxygen                   Metalimnetic oygen 
#>                         "CHM_oxyepi"                         "CHM_oxymet" 
#>                  Hypolimnetic oxygen           Metalimnetic oxygen minima 
#>                         "CHM_oxyhyp"                         "CHM_oxymom" 
#>              Number of anoxic layers  Photosynthetically active radiation 
#>                         "CHM_oxynal"                            "RAD_par" 
#>              Particulate inorganic P                     Suspended solids 
#>                            "PHS_pip"                            "NCS_ss1" 
#>                          Zooplankton                      Air temperature 
#>                           "ZOO_zoo1"                         "MET_tmpair" 
#>     Water-air temperature difference 
#>                         "HYD_atdiff" 
```
