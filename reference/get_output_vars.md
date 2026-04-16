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
#> Error in dplyr::select(dplyr::bind_rows(outf), Date, HYD_flow): Can't select columns that don't exist.
#> ✖ Column `HYD_flow` doesn't exist.
# Run models
aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
path = path, model_controls = model_controls,
parallel = TRUE, ncores = 2L)
#> Error in run_aeme(aeme = aeme, model = model, verbose = FALSE, path = path,     model_controls = model_controls, parallel = TRUE, ncores = 2L): ✖ Simulation folder does not exist
#>   C:\Users\runneradmin\AppData\Local\Temp\RtmpwL0wRX/45819_wainamu
get_output_vars(aeme, model)
#> NULL
```
