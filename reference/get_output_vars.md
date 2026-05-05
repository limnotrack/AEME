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
#> ℹ All columns already match AEME standard variable names, skipping name
#>   guessing.
#> ℹ MET_tmpair: values appear to be in the expected units, no conversion applied.
#> ℹ MET_tmpdew: values appear to be in the expected units, no conversion applied.
#> ℹ MET_radswd: values appear to be in the expected units, no conversion applied.
#> ℹ MET_radlwd: values appear to be in the expected units, no conversion applied.
#> ℹ MET_humrel: values appear to be in the expected units, no conversion applied.
#> ℹ MET_cldcvr: values appear to be in the expected units, no conversion applied.
#> ℹ MET_prsttn: values appear to be in the expected units, no conversion applied.
#> ℹ MET_prmslp: values appear to be in the expected units, no conversion applied.
#> ℹ MET_prvapr: values appear to be in the expected units, no conversion applied.
#> ℹ MET_wndspd: values appear to be in the expected units, no conversion applied.
#> ℹ MET_wnduvu: values appear to be in the expected units, no conversion applied.
#> ℹ MET_wnduvv: values appear to be in the expected units, no conversion applied.
#> ℹ MET_pprain: values appear to be in the expected units, no conversion applied.
#> ℹ MET_ppsnow: values appear to be in the expected units, no conversion applied.
#> ℹ All columns already match AEME standard inflow variable names, skipping name
#>   guessing.
#> ℹ `HYD_temp`: values appear to be in the expected units, no conversion applied.
#> ℹ `CHM_oxy`: values appear to be in the expected units, no conversion applied.
#> ℹ `NIT_amm`: values appear to be in the expected units, no conversion applied.
#> ℹ `NIT_nit`: values appear to be in the expected units, no conversion applied.
#> ℹ `NIT_don`: values appear to be in the expected units, no conversion applied.
#> ℹ `NIT_pon`: values appear to be in the expected units, no conversion applied.
#> ℹ `PHS_frp`: values appear to be in the expected units, no conversion applied.
#> ℹ `PHS_dop`: values appear to be in the expected units, no conversion applied.
#> ℹ `PHS_pop`: values appear to be in the expected units, no conversion applied.
#> ℹ `PHS_pip`: values appear to be in the expected units, no conversion applied.
#> ℹ `CAR_doc`: values appear to be in the expected units, no conversion applied.
#> ℹ `CAR_poc`: values appear to be in the expected units, no conversion applied.
#> ℹ `SIL_rsi`: values appear to be in the expected units, no conversion applied.
#> Warning: ! `SIL_rsi`: SIL_rsi is constant across all rows — this may be a placeholder
#>   value.
#> ℹ Check raw data or unit conversion for this variable.
#> Warning: ! 1 missing state variable in `FWMT`:
#> ✖ `ZOO_zoo1 `
#> ℹ Filled 1 missing variable with default value from `model_controls`.
#> ℹ Using observed water level.
#> ! Missing values in observed water level.
#> ℹ Correcting water balance using estimated outflows (method = 2).
#> ℹ Calculating lake level using lake depth and a sinisoidal function.
#> ℹ Building GLM-AED for lake wainamu
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
#> ℹ PHY_cyano 10 replaced with 0.24022
#> ℹ PHY_diatom 8.4 replaced with 0.300275
#> ℹ PHY_green 0.04 replaced with 0.300275
#> ℹ 100 replaced with 1
#> ℹ Using default zooplankton initialisation
#> ✔ Updated GLM-AED models from: aed_sedflux, aed_oxygen, aed_silica,
#>   aed_nitrogen, aed_phosphorus, aed_organic_matter, aed_phytoplankton,
#>   aed_zooplankton, aed_macrophyte, aed_totals to: aed_sedflux, aed_oxygen,
#>   aed_silica, aed_nitrogen, aed_phosphorus, aed_organic_matter,
#>   aed_phytoplankton, aed_totals
#> ℹ Setting up AED aed_sed_const2d sediment zones: 2
#> 
#> Tier 2: zone-median summer concentrations used for adjustment:
#>         oxy   amm   nit   frp
#> Zone1 0.075 0.078 0.010 0.004
#> Zone2 7.160 0.005 0.001 0.002
#> Tier 2 adjustments applied: fsed_amm (2 zones, direct NH4); fsed_frp (2 zones, direct FRP)
#> 
#> === Sediment zone flux estimates (obs_adjusted) ===
#> n_zones: 2 | max lake depth: 13.07 m | ref_depth: 5 m
#> 
#>  zone height_lower_m height_upper_m depth_upper_m depth_lower_m mean_depth_m
#>     1           0.00           3.07            10          13.1         11.5
#>     2           3.07          19.00             0          10.0          5.0
#>  area_m2 area_frac fsed_oxy fsed_amm fsed_nit fsed_frp
#>    43957     0.289    -38.8    5.835     -0.4   0.1035
#>   108386     0.711    -19.4    0.512      0.1   0.0259
#> 
#> Lake-wide area-weighted average fluxes (for sanity check):
#>     oxy     amm     nit     frp 
#> -25.007   2.050  -0.044   0.048 
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
#> ℹ Running models in parallel... [2026-05-05 04:39:12]
#> ✔ Model run complete! [2026-05-05 04:39:15]
#> ℹ Reading models in parallel...[2026-05-05 04:39:15]
#> ✔ Model reading complete! [2026-05-05 04:39:17]
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
#>                           Lake depth                        Water density 
#>                         "LKE_depths"                           "HYD_dens" 
#>                           Stratified                    Schmidt stability 
#>                          "HYD_strat"                         "HYD_schstb" 
#>                   Centre of buoyancy                     Epilimnion depth 
#>                         "HYD_ctrbuy"                         "HYD_epidep" 
#>                    Hypolimnion depth                             Salinity 
#>                         "HYD_hypdep"                           "CHM_salt" 
#>                       Oxycline depth                   Epilimnetic oxygen 
#>                         "CHM_oxycln"                         "CHM_oxyepi" 
#>                   Metalimnetic oygen                  Hypolimnetic oxygen 
#>                         "CHM_oxymet"                         "CHM_oxyhyp" 
#>           Metalimnetic oxygen minima              Number of anoxic layers 
#>                         "CHM_oxymom"                         "CHM_oxynal" 
#>    Trophic Level Index Chlorophyll-a   Trophic Level Index Total Nitrogen 
#>                           "LKE_tlic"                           "LKE_tlin" 
#> Trophic Level Index Total Phosphorus     Trophic Level Index Secchi depth 
#>                           "LKE_tlip"                          "LKE_tlise" 
#>                Trophic Level Index 3                Trophic Level Index 4 
#>                           "LKE_tli3"                           "LKE_tli4" 
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
#>  Photosynthetically active radiation              Particulate inorganic P 
#>                            "RAD_par"                            "PHS_pip" 
#>                     Suspended solids                          Zooplankton 
#>                            "NCS_ss1"                           "ZOO_zoo1" 
#>                      Air temperature     Water-air temperature difference 
#>                         "MET_tmpair"                         "HYD_atdiff" 
```
