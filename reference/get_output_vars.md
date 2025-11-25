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
#> ℹ Using observed water level
#> ! Missing values in observed water level
#> ℹ Insufficient water level observations. Using constant water level
#> ℹ Correcting water balance using estimated outflows (method = 2).
#> ℹ Calculating lake level using lake depth and a sinisoidal function.
#> ℹ Building GLM-AED2 for lake wainamu
#> ℹ Copied in GLM nml file
#> ℹ Copied in AED nml file
#> ℹ  oxy_initial = 625 replaced with 312.5
#> ℹ  frp_initial = 0.3229 replaced with 0.3229
#> ℹ  dop_initial = 0.3229 replaced with 0.3229
#> ℹ  pop_initial = 0.3229 replaced with 0.3229
#> ℹ  amm_initial = 1.4279 replaced with 1.4279
#> ℹ  nit_initial = 1.0709 replaced with 1.0709
#> ℹ  don_initial = 21.4183 replaced with 21.4183
#> ℹ  pon_initial = 7.1394 replaced with 7.1394
#> ℹ  doc_initial = 41.6285 replaced with 41.6285
#> ℹ  poc_initial = 16.6514 replaced with 16.6514
#> ℹ  rsi_initial = 1 replaced with 1
#> ℹ PHY_cyano 0.24022 replaced with 0.24022
#> ℹ PHY_green 0.300275 replaced with 0.300275
#> ℹ PHY_diatom 0.300275 replaced with 0.300275
#> ℹ  ss_initial = 3,3 replaced with 3,
#> ℹ Building GOTM-WET model for lake wainamu
#> ℹ Copied in GOTM configuration files
#> ℹ instances/abiotic_water/initialization/sO2W 13 replaced with 10
#> ℹ instances/abiotic_water/initialization/sPO4W 0.1 replaced with 0.01
#> ℹ instances/abiotic_water/initialization/sPDOMW 0.001 replaced with 0.01
#> ℹ instances/abiotic_water/initialization/sPPOMW 0.001 replaced with 0.01
#> ℹ instances/abiotic_water/initialization/sNH4W 0.05 replaced with 0.02
#> ℹ instances/abiotic_water/initialization/sNO3W 0.5 replaced with 0.015
#> ℹ instances/abiotic_water/initialization/sNDOMW 0.01 replaced with 0.3
#> ℹ instances/abiotic_water/initialization/sNPOMW 0.01 replaced with 0.1
#> ℹ instances/abiotic_water/initialization/sDDOMW 2.5 replaced with 0.5
#> ℹ instances/abiotic_water/initialization/sDPOMW 0.1 replaced with 0.2
#> ℹ instances/abiotic_water/initialization/sSiO2W 3.5 replaced with 1
#> ℹ Setting initial condition for instances/cyanobacteria/initialization/sDW: 0.1
#>   replaced with 0.2
#> ℹ Setting initial condition for instances/cyanobacteria/initialization/sNW:
#>   0.03 replaced with 0.03
#> ℹ Setting initial condition for instances/cyanobacteria/initialization/sPW:
#>   0.003 replaced with 0.0019
#> ℹ Setting initial condition for instances/greens/initialization/sDW: 0.1
#>   replaced with 0.1
#> ℹ Setting initial condition for instances/greens/initialization/sNW: 0.05
#>   replaced with 0.015
#> ℹ Setting initial condition for instances/greens/initialization/sPW: 0.001
#>   replaced with 0.00094
#> ℹ Setting initial condition for instances/diatoms/initialization/sDW: 0.2
#>   replaced with 0.25
#> ℹ Setting initial condition for instances/diatoms/initialization/sNW: 0.05
#>   replaced with 0.038
#> ℹ Setting initial condition for instances/diatoms/initialization/sPW: 0.005
#>   replaced with 0.0024
#> ℹ instances/abiotic_water/initialization/sDIMW 4 replaced with 3
#> ✔ GOTM YAML validation completed - no issues detected.
#> ✔ GLM nml validation completed - no issues detected.
# Run models
aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
path = path, model_controls = model_controls,
parallel = TRUE, ncores = 2L)
#> ℹ Running models in parallel... [2025-11-25 20:25:58]
#> ✔ Model run complete! [2025-11-25 20:26:03]
#> ℹ Reading models in parallel...[2025-11-25 20:26:03]
#> ✔ Model reading complete! [2025-11-25 20:26:04]
get_output_vars(aeme, model)
#>                Water temperature                 Dissolved oxygen 
#>                       "HYD_temp"                        "CHM_oxy" 
#>              Total chlorophyll a                   Total nitrogen 
#>                      "PHY_tchla"                         "NIT_tn" 
#>                 Total phosphorus            Evaporative heat flux 
#>                         "PHS_tp"                         "LKE_Qe" 
#>               Sensible heat flux               Longwave radiation 
#>                         "LKE_Qh"                        "LKE_Qlw" 
#>              Shortwave radiation                           Volume 
#>                        "LKE_Qsw"                          "LKE_V" 
#>                      Evaporation                      Evaporation 
#>                     "LKE_evpvol"                     "LKE_evpflx" 
#>                     Surface area                      Evaporation 
#>                         "LKE_A0"                     "LKE_evprte" 
#>                           Inflow                          Outflow 
#>                     "LKE_inflow"                    "LKE_outflow" 
#>                    Precipitation                    Precipitation 
#>                     "LKE_precip"                     "LKE_pcpvol" 
#>   Remote sensed skin temperature                             Time 
#>                      "HYD_surft"                           "Date" 
#>                       Lake depth                         Salinity 
#>                     "LKE_depths"                       "CHM_salt" 
#>                        Phosphate              Dissolved organic P 
#>                        "PHS_frp"                        "PHS_dop" 
#>            Particulate organic P          Particulate inorganic P 
#>                        "PHS_pop"                        "PHS_pip" 
#>              Ammoniacal nitrogen                          Nitrate 
#>                        "NIT_amm"                        "NIT_nit" 
#>              Dissolved organic N            Particulate organic N 
#>                        "NIT_don"                        "NIT_pon" 
#>         Dissolved organic carbon       Particulate organic carbon 
#>                        "CAR_doc"                        "CAR_poc" 
#>                    Cyanobacteria                      Green algae 
#>                      "PHY_cyano"                      "PHY_green" 
#>               Diatoms freshwater                 Suspended solids 
#>                     "PHY_diatom"                        "NCS_ss1" 
#>                  Air temperature Water-air temperature difference 
#>                     "MET_tmpair"                     "HYD_atdiff" 
```
