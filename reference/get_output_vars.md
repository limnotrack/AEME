# Get the output variables from an AEME object

Get the output variables from an AEME object

## Usage

``` r
get_output_vars(
  aeme,
  model,
  ens_n = 1,
  group = c("core", "diag", "sediment")[1]
)
```

## Arguments

- aeme:

  Aeme object.

- model:

  character vector; models to use. One or more of `"dy_cd"`,
  `"glm_aed"`, `"gotm_wet"`, `"simstrat_aed2"`. Defaults to all models
  if not found in `aeme`.

- ens_n:

  numeric; ensemble number to allocate to model output which is loaded.
  Defaults to 1. A run with `ens_n = 1` clears any existing output; a
  run with another number adds (or replaces) that member and keeps the
  rest.

- group:

  character; which variables to list: `"core"` (time and depth x time
  variables, default), `"diag"` (diagnostics and scalars), and/or
  `"sediment"` (sediment zone/layer variables). Diagnostic and sediment
  variables keep their raw model names.

## Value

A character vector of the output variables

## Examples

``` r
aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
aeme <- readRDS(aeme_file)
path <- tempdir()
model_controls <- get_model_controls(use_bgc = TRUE)
model <- c("glm_aed")
aeme <- build_aeme(path = path, aeme = aeme, model = model,
                   model_controls = model_controls,
                   ext_elev = 5, use_bgc = TRUE)
#> Warning: ! `SIL_rsi`: SIL_rsi is constant across all rows -- this may be a placeholder
#>   value.
#> ℹ Check raw data or unit conversion for this variable.
#> 
#> ── Calculating water balance ──
#> 
#> Resolving water level
#>   ℹ Using observed water level
#> ! Missing values in observed water level
#> ℹ Estimating surface water temperature
#> ✔ Estimating surface water temperature [24ms]
#> 
#> Estimating lake water levels for glm_aed
#>   ℹ Optimizing parameters for water balance
#>   ✔ Optimization Complete: C = 0.3343, h_inv = 23.4915, Final RMSE = 0.1431
#> ℹ Correcting water balance using estimated outflows (method = 2).
#> 
#> ── Building GLM-AED for lake wainamu ──
#> 
#> ℹ Aligned AED sediment zones to GLM: 2 zones (all active).
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
#> ! Fitted weir crest (23.49 m) is below the initial lake level; using 23.64 m.
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
#> ┌────┬───────────┬───────────┬───────────┬───────────┬──────────┬─────────┬─────────┬─────┬────┬────┬──────┐
#> │Zone│H lower (m)│H upper (m)│D upper (m)│D lower (m)│Mean D (m)│Area (m2)│Area frac│ O2  │ NH4│ NO3│ FRP  │
#> ├────┼───────────┼───────────┼───────────┼───────────┼──────────┼─────────┼─────────┼─────┼────┼────┼──────┤
#> │   1│    0      │ 3.07      │   10      │ 13.1      │ 11.5     │ 4.4e+04 │ 0.289   │-38.8│ 5.7│-0.4│ 0.107│
#> │   2│ 3.07      │   19      │    0      │   10      │    5     │ 1.08e+05│ 0.711   │-19.4│ 0.5│ 0.1│0.0268│
#> └────┴───────────┴───────────┴───────────┴───────────┴──────────┴─────────┴─────────┴─────┴────┴────┴──────┘
#> 
#> ── Lake-wide area-weighted average fluxes (mmol/m2/d) ──────────────────────────
#> ┌──────────────┬───────────────┬───────────────┬───────────────┐
#> │O2 (mmol/m2/d)│NH4 (mmol/m2/d)│NO3 (mmol/m2/d)│FRP (mmol/m2/d)│
#> ├──────────────┼───────────────┼───────────────┼───────────────┤
#> │ -25.007      │ 2.002         │ -0.044        │ 0.05          │
#> └──────────────┴───────────────┴───────────────┴───────────────┘
#> ✔ GLM nml validation completed - no issues detected.
# Run models
aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
path = path, model_controls = model_controls)
#> ℹ Running models... (Have you tried parallelizing?) [2026-10-29 23:28:05]
#> ℹ GLM-AED running... [2026-10-29 23:28:05]
#> ✔ GLM-AED running... [2026-10-29 23:28:08] [2.7s]
#> 
#> ✔ Model run complete! [2026-10-29 23:28:08]
get_output_vars(aeme, model)
#>     Water temperature     Thermocline depth      Dissolved oxygen 
#>            "HYD_temp"          "HYD_thmcln"             "CHM_oxy" 
#>   Total chlorophyll a        Total nitrogen      Total phosphorus 
#>           "PHY_tchla"              "NIT_tn"              "PHS_tp" 
#>           Water level Evaporative heat flux    Sensible heat flux 
#>          "LKE_lvlwtr"              "LKE_Qe"              "LKE_Qh" 
#>    Longwave radiation   Shortwave radiation                Volume 
#>             "LKE_Qlw"             "LKE_Qsw"             "LKE_vol" 
#>           Evaporation           Evaporation          Surface area 
#>          "LKE_evpvol"          "LKE_evpflx"              "LKE_A0" 
#>           Evaporation                Inflow              Overflow 
#>          "LKE_evprte"          "LKE_inflow"        "LKE_overflow" 
#>               Outflow         Total outflow         Precipitation 
#>         "LKE_outflow"         "LKE_outftot"          "LKE_precip" 
#>         Precipitation   Surface temperature            Lake depth 
#>          "LKE_pcpvol"           "HYD_surft"          "LKE_depths" 
#>                     z         Water density            Stratified 
#>                   "z"            "HYD_dens"           "HYD_strat" 
#>              Salinity             Phosphate   Dissolved organic P 
#>            "CHM_salt"             "PHS_frp"             "PHS_dop" 
#> Particulate organic P   Ammoniacal nitrogen               Nitrate 
#>             "PHS_pop"             "NIT_amm"             "NIT_nit" 
#>   Dissolved organic N Particulate organic N   Dissolved organic C 
#>             "NIT_don"             "NIT_pon"             "CAR_doc" 
#> Particulate organic C               SIL_rsi         Cyanobacteria 
#>             "CAR_poc"             "SIL_rsi"           "PHY_cyano" 
#>           Green algae    Diatoms freshwater      Suspended solids 
#>           "PHY_green"          "PHY_diatom"             "NCS_ss1" 
#>      Suspended solids                    ok 
#>             "NCS_ss2"                  "ok" 
```
