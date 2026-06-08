# Get the output variables from an AEME object

Get the output variables from an AEME object

## Usage

``` r
get_output_vars(aeme, model, ens_n = 1)
```

## Arguments

- aeme:

  Aeme object.

- model:

  character vector; models to use. One or more of `"dy_cd"`,
  `"glm_aed"`, `"gotm_wet"`. Defaults to all models if not found in
  `aeme`.

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
#> ℹ Detected regular timestep: 1 day(s).
#> ℹ Detected regular timestep: 1 day(s).
#> Warning: ! `SIL_rsi`: SIL_rsi is constant across all rows — this may be a placeholder
#>   value.
#> ℹ Check raw data or unit conversion for this variable.
#> 
#> ── Calculating water balance ──
#> 
#> Resolving water level
#>   ℹ Using observed water level
#> ! Missing values in observed water level
#> ℹ Estimating surface water temperature
#> ✔ Estimating surface water temperature [7ms]
#> 
#> Estimating lake water levels for glm_aed
#>   ℹ Optimizing parameters for water balance
#>   ✔ Optimization Complete: C = 0.3343, h_inv = 23.4915, Final RMSE = 0.1431
#> Estimating lake water levels for gotm_wet
#>   ℹ Optimizing parameters for water balance
#>   ✔ Optimization Complete: C = 0.334, h_inv = 23.4829, Final RMSE = 0.1472
#> ℹ Correcting water balance using estimated outflows (method = 2).
#> 
#> ── Building GLM-AED for lake wainamu ──
#> 
#> ℹ Copied in GLM nml file
#> ℹ Copied in AED nml file and supporting files
#> ℹ Copied in GLM plots nml file
#> ℹ CAR_doc: 15 replaced with 41.6285
#> ℹ CAR_poc: 15 replaced with 16.6514
#> ℹ CHM_oxy: 225 replaced with 312.5
#> ℹ NIT_amm: 2.25 replaced with 1.4279
#> ℹ NIT_don: 21 replaced with 21.4183
#> ℹ NIT_nit: 6.96 replaced with 1.0709
#> ℹ NIT_pon: 19.8 replaced with 7.1394
#> ℹ PHS_dop: 0.008 replaced with 0.3229
#> ℹ PHS_frp: 0.05 replaced with 0.3229
#> ℹ PHS_pop: 0.05 replaced with 0.3229
#> ℹ PHY_cyano: 10 replaced with 0.24022
#> ℹ PHY_diatom: 8.4 replaced with 0.300275
#> ℹ PHY_green: 0.04 replaced with 0.300275
#> ℹ SIL_rsi: 100 replaced with 1
#> ✔ Updated GLM-AED models from: aed_sedflux, aed_oxygen, aed_silica,
#>   aed_nitrogen, aed_phosphorus, aed_organic_matter, aed_phytoplankton,
#>   aed_zooplankton, aed_macrophyte, aed_totals to: aed_sedflux, aed_oxygen,
#>   aed_silica, aed_nitrogen, aed_phosphorus, aed_organic_matter,
#>   aed_phytoplankton, aed_totals
#> ℹ Setting up AED aed_sed_const2d sediment zones: 2
#> ℹ Tier 2: zone-median summer concentrations used for adjustment:
#> ┌─────┬───────────┬────────────┬────────────┬────────────┐
#> │ zone│ O2 (mg/L) │ NH4 (mg/L) │ NO3 (mg/L) │ FRP (mg/L) │
#> ├─────┼───────────┼────────────┼────────────┼────────────┤
#> │Zone1│ 0.075     │ 0.078      │ 0.01       │ 0.004      │
#> │Zone2│ 7.16      │ 0.005      │ 0.001      │ 0.002      │
#> └─────┴───────────┴────────────┴────────────┴────────────┘
#> ℹ Tier 2 adjustments applied: fsed_amm (2 zones, direct NH4); fsed_frp (2
#>   zones, direct FRP)
#> ── Sediment zone flux estimates (obs_adjusted) ─────────────────────────────────
#> n_zones: 2 | max lake depth: 13.07 m | ref_depth: 5 m
#> ┌────┬───────────┬───────────┬───────────┬───────────┬──────────┬─────────┬─────────┬─────┬─────┬────┬──────┐
#> │Zone│H lower (m)│H upper (m)│D upper (m)│D lower (m)│Mean D (m)│Area (m2)│Area frac│ O2  │ NH4 │ NO3│ FRP  │
#> ├────┼───────────┼───────────┼───────────┼───────────┼──────────┼─────────┼─────────┼─────┼─────┼────┼──────┤
#> │   1│    0      │ 3.07      │   10      │ 13.1      │ 11.5     │ 4.4e+04 │ 0.289   │-38.8│ 5.83│-0.4│ 0.103│
#> │   2│ 3.07      │   19      │    0      │   10      │    5     │ 1.08e+05│ 0.711   │-19.4│0.512│ 0.1│0.0259│
#> └────┴───────────┴───────────┴───────────┴───────────┴──────────┴─────────┴─────────┴─────┴─────┴────┴──────┘
#> 
#> ── Lake-wide area-weighted average fluxes ──────────────────────────────────────
#> ┌──────────────┬───────────────┬───────────────┬───────────────┐
#> │O2 (mmol/m2/d)│NH4 (mmol/m2/d)│NO3 (mmol/m2/d)│FRP (mmol/m2/d)│
#> ├──────────────┼───────────────┼───────────────┼───────────────┤
#> │ -25.007      │ 2.05          │ -0.044        │ 0.048         │
#> └──────────────┴───────────────┴───────────────┴───────────────┘
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
#> ✔ GOTM YAML validation completed - no issues detected.
#> ✔ GLM nml validation completed - no issues detected.
# Run models
aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
path = path, model_controls = model_controls,
parallel = TRUE, ncores = 2L)
#> ℹ Running models in parallel... [2026-06-08 23:42:14]
#> ✔ Model run complete! [2026-06-08 23:42:17]
#> ℹ Reading models in parallel...[2026-06-08 23:42:17]
#> ✔ Model reading complete! [2026-06-08 23:42:18]
get_output_vars(aeme, model)
#>                   Water temperature                   Thermocline depth 
#>                          "HYD_temp"                        "HYD_thmcln" 
#>                    Dissolved oxygen                 Total chlorophyll a 
#>                           "CHM_oxy"                         "PHY_tchla" 
#>                      Total nitrogen                    Total phosphorus 
#>                            "NIT_tn"                            "PHS_tp" 
#>               Evaporative heat flux                  Sensible heat flux 
#>                            "LKE_Qe"                            "LKE_Qh" 
#>                  Longwave radiation                 Shortwave radiation 
#>                           "LKE_Qlw"                           "LKE_Qsw" 
#>                              Volume                         Evaporation 
#>                             "LKE_V"                        "LKE_evpvol" 
#>                         Evaporation                        Surface area 
#>                        "LKE_evpflx"                            "LKE_A0" 
#>                         Evaporation                              Inflow 
#>                        "LKE_evprte"                        "LKE_inflow" 
#>                            Overflow                             Outflow 
#>                      "LKE_overflow"                       "LKE_outflow" 
#>                       Total outflow                       Precipitation 
#>                       "LKE_outftot"                        "LKE_precip" 
#>                       Precipitation      Remote sensed skin temperature 
#>                        "LKE_pcpvol"                         "HYD_surft" 
#>                          Lake depth                       Water density 
#>                        "LKE_depths"                          "HYD_dens" 
#>                          Stratified                            Salinity 
#>                         "HYD_strat"                          "CHM_salt" 
#>                           Phosphate                 Dissolved organic P 
#>                           "PHS_frp"                           "PHS_dop" 
#>               Particulate organic P                 Ammoniacal nitrogen 
#>                           "PHS_pop"                           "NIT_amm" 
#>                             Nitrate                 Dissolved organic N 
#>                           "NIT_nit"                           "NIT_don" 
#>               Particulate organic N            Dissolved organic carbon 
#>                           "NIT_pon"                           "CAR_doc" 
#>          Particulate organic carbon                       Cyanobacteria 
#>                           "CAR_poc"                         "PHY_cyano" 
#>                         Green algae                  Diatoms freshwater 
#>                         "PHY_green"                        "PHY_diatom" 
#> Photosynthetically active radiation             Particulate inorganic P 
#>                           "RAD_par"                           "PHS_pip" 
#>                    Suspended solids                     Air temperature 
#>                           "NCS_ss1"                        "MET_tmpair" 
#>    Water-air temperature difference 
#>                        "HYD_atdiff" 
```
