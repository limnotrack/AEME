# Get the variables that are both in the observation and model output

Get the variables that are both in the observation and model output

## Usage

``` r
list_mod_obs_vars(aeme, model, ens_n = 1)
```

## Arguments

- aeme:

  aeme; object.

- model:

  vector; of models to be used. Can be `dy_cd`, `glm_aed`, `gotm_wet`.

- ens_n:

  numeric; ensemble number to allocate to model output which is loaded.
  Defaults to 1.

## Value

A character vector of variables that are in both the observation and
model output

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
#> ℹ All columns already match AEME standard variable names, skipping name
#>   guessing.
#> ℹ All columns already match AEME standard inflow variable names, skipping name
#>   guessing.
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
#> ℹ 41.6285 replaced with 41.6285
#> ℹ Using default pH initialisation
#> ℹ 16.6514 replaced with 16.6514
#> ℹ 312.5 replaced with 312.5
#> ℹ 1.4279 replaced with 1.4279
#> ℹ 21.4183 replaced with 21.4183
#> ℹ 1.0709 replaced with 1.0709
#> ℹ 7.1394 replaced with 7.1394
#> ℹ 0.3229 replaced with 0.3229
#> ℹ 0.3229 replaced with 0.3229
#> ℹ 0.3229 replaced with 0.3229
#> ℹ PHY_cyano 0.24 replaced with 0.24022
#> ℹ PHY_diatom 0.3 replaced with 0.300275
#> ℹ PHY_green 0.3 replaced with 0.300275
#> ℹ 1 replaced with 1
#> ℹ Using default zooplankton initialisation
#> ✔ Updated GLM-AED models from: aed_sedflux, aed_oxygen, aed_silica,
#>   aed_nitrogen, aed_phosphorus, aed_organic_matter, aed_phytoplankton,
#>   aed_totals to: aed_sedflux, aed_oxygen, aed_silica, aed_nitrogen,
#>   aed_phosphorus, aed_organic_matter, aed_phytoplankton, aed_totals
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
#> ✔ GLM nml validation completed - no issues detected.
# Run models
aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
path = path, model_controls = model_controls,
parallel = TRUE, ncores = 2L)
#> ℹ Running models in parallel... [2026-05-17 21:37:18]
#> ✔ Model run complete! [2026-05-17 21:37:21]
#> ℹ Reading models in parallel...[2026-05-17 21:37:21]
#> ✔ Model reading complete! [2026-05-17 21:37:22]
aeme |> 
  list_mod_obs_vars()
#>              Cyanobacteria           Dissolved oxygen 
#>                "PHY_cyano"                  "CHM_oxy" 
#>                   Salinity          Water temperature 
#>                 "CHM_salt"                 "HYD_temp" 
#>        Total chlorophyll a   Dissolved organic carbon 
#>                "PHY_tchla"                  "CAR_doc" 
#>                  Phosphate        Ammoniacal nitrogen 
#>                  "PHS_frp"                  "NIT_amm" 
#>                    Nitrate             Total nitrogen 
#>                  "NIT_nit"                   "NIT_tn" 
#>           Total phosphorus          Thermocline depth 
#>                   "PHS_tp"               "HYD_thmcln" 
#>                 Stratified         Centre of buoyancy 
#>                "HYD_strat"               "HYD_ctrbuy" 
#>           Epilimnion depth          Hypolimnion depth 
#>               "HYD_epidep"               "HYD_hypdep" 
#>          Schmidt stability             Oxycline depth 
#>               "HYD_schstb"               "CHM_oxycln" 
#>         Epilimnetic oxygen         Metalimnetic oygen 
#>               "CHM_oxyepi"               "CHM_oxymet" 
#> Metalimnetic oxygen minima    Number of anoxic layers 
#>               "CHM_oxymom"               "CHM_oxynal" 
#>      Trophic Level Index 3      Trophic Level Index 4 
#>                 "LKE_tli3"                 "LKE_tli4" 
```
