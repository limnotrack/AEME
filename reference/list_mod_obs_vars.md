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

  vector; of models to be used. Can be \`dy_cd\`, \`glm_aed\`,
  \`gotm_wet\`.

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
#> ℹ Using observed water level
#> ! Missing values in observed water level
#> ℹ Insufficient water level observations. Using constant water level
#> ℹ Correcting water balance using estimated outflows (method = 2).
#> ℹ Calculating lake level using lake depth and a sinisoidal function.
#> ℹ Building GLM-AED2 for lake wainamu
#> ℹ Copied in AED nml file and supporting files
#> ℹ 312.5 replaced with 312.5
#> ℹ 0.3229 replaced with 0.3229
#> ℹ 0.3229 replaced with 0.3229
#> ℹ 0.3229 replaced with 0.3229
#> ℹ 1.4279 replaced with 1.4279
#> ℹ 1.0709 replaced with 1.0709
#> ℹ 21.4183 replaced with 21.4183
#> ℹ 7.1394 replaced with 7.1394
#> ℹ 41.6285 replaced with 41.6285
#> ℹ 16.6514 replaced with 16.6514
#> ℹ 1 replaced with 1
#> ℹ PHY_cyano 0.24022 replaced with 0.24022
#> ℹ PHY_green 0.300275 replaced with 0.300275
#> ℹ PHY_diatom 0.300275 replaced with 0.300275
#> ✔ GLM nml validation completed - no issues detected.
# Run models
aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
path = path, model_controls = model_controls,
parallel = TRUE, ncores = 2L)
#> ℹ Running models in parallel... [2025-12-10 20:20:21]
#> ✔ Model run complete! [2025-12-10 20:20:25]
#> ℹ Reading models in parallel...[2025-12-10 20:20:25]
#> ✔ Model reading complete! [2025-12-10 20:20:25]
list_mod_obs_vars(aeme = aeme, model = model)
#>            Cyanobacteria         Dissolved oxygen                 Salinity 
#>              "PHY_cyano"                "CHM_oxy"               "CHM_salt" 
#>        Water temperature      Total chlorophyll a Dissolved organic carbon 
#>               "HYD_temp"              "PHY_tchla"                "CAR_doc" 
#>                Phosphate      Ammoniacal nitrogen                  Nitrate 
#>                "PHS_frp"                "NIT_amm"                "NIT_nit" 
#>           Total nitrogen         Total phosphorus 
#>                 "NIT_tn"                 "PHS_tp" 
```
