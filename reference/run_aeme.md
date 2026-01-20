# Run aquatic model ensemble

Run aquatic model ensemble

## Usage

``` r
run_aeme(
  aeme,
  model,
  return_type = c("aeme", "exec_result", "both", "none"),
  ens_n = 1,
  model_controls = NULL,
  verbose = FALSE,
  debug = FALSE,
  timeout = Inf,
  parallel = FALSE,
  ncores,
  check_output = FALSE,
  path = "."
)
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

- model_controls:

  dataframe; of configuration loaded from "model_controls.csv".

- verbose:

  logical; print model output to console. Defaults to FALSE.

- debug:

  logical; write debug log (Only DYRESM). Defaults to FALSE.

- timeout:

  Timeout for the process, in seconds, or as a `difftime` object. If it
  is not finished before this, it will be killed.

- parallel:

  logical; run models in parallel. Defaults to FALSE.

- ncores:

  integer; number of cores to use for parallelization. Defaults to
  \`min(c(detectCores() - 1, length(model)))\`.

- check_output:

  logical; check model output after running? Defaults to FALSE.

- path:

  filepath; where input files are located relative to the current
  working directory.

- return:

  logical; return model output within an \`aeme\` object? Defaults to
  TRUE.

## Value

an \`aeme\` object with model output loaded.

## Examples

``` r
aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
aeme <- readRDS(aeme_file)
path <- tempdir()
model_controls <- get_model_controls()
model <- c("glm_aed")
aeme <- build_aeme(path = path, aeme = aeme, model = model,
model_controls = model_controls, ext_elev = 5)
#> ℹ Using observed water level
#> ℹ No missing values in observed water level. Using observed water level
#> Parameters: C = 0.5 , h_inv = 22.9904 

#> Parameters: C = 0.5 , h_inv = 22.9904 
#> Parameters: C = 0.501 , h_inv = 22.9904 
#> Parameters: C = 0.499 , h_inv = 22.9904 
#> Parameters: C = 0.5 , h_inv = 22.9914 
#> Parameters: C = 0.5 , h_inv = 22.9894 
#> Parameters: C = 0.2661 , h_inv = 23.6504 
#> Parameters: C = 0.2671 , h_inv = 23.6504 
#> Parameters: C = 0.2651 , h_inv = 23.6504 
#> Parameters: C = 0.2661 , h_inv = 23.6504 
#> Parameters: C = 0.2661 , h_inv = 23.6494 
#> Parameters: C = 0.3939 , h_inv = 23.3644 
#> Parameters: C = 0.3949 , h_inv = 23.3644 
#> Parameters: C = 0.3929 , h_inv = 23.3644 
#> Parameters: C = 0.3939 , h_inv = 23.3654 
#> Parameters: C = 0.3939 , h_inv = 23.3634 
#> Parameters: C = 0.3311 , h_inv = 23.5049 
#> Parameters: C = 0.3321 , h_inv = 23.5049 
#> Parameters: C = 0.3301 , h_inv = 23.5049 
#> Parameters: C = 0.3311 , h_inv = 23.5059 
#> Parameters: C = 0.3311 , h_inv = 23.5039 
#> Parameters: C = 0.3395 , h_inv = 23.4794 
#> Parameters: C = 0.3405 , h_inv = 23.4794 
#> Parameters: C = 0.3385 , h_inv = 23.4794 
#> Parameters: C = 0.3395 , h_inv = 23.4804 
#> Parameters: C = 0.3395 , h_inv = 23.4784 
#> Parameters: C = 0.3355 , h_inv = 23.4916 
#> Parameters: C = 0.3365 , h_inv = 23.4916 
#> Parameters: C = 0.3345 , h_inv = 23.4916 
#> Parameters: C = 0.3355 , h_inv = 23.4926 
#> Parameters: C = 0.3355 , h_inv = 23.4906 
#> Optimization Complete:
#>   Best C: 0.3355
#>   Best h_inv: 23.4916
#>   Final RMSE: 0.1397

#> ℹ Correcting water balance using estimated outflows (method = 2).
#> ℹ Calculating lake level using lake depth and a sinisoidal function.
#> ℹ Building GLM-AED2 for lake wainamu
#> ✔ GLM nml validation completed - no issues detected.
aeme <- run_aeme(aeme = aeme, model = model, path = path)
#> ℹ Running models... (Have you tried parallelizing?) [2026-01-20 20:36:22]
#> → GLM-AED running... [2026-01-20 20:36:22]
#> ✔ GLM-AED run successful! [2026-01-20 20:36:22]
#> ✔ Model run complete! [2026-01-20 20:36:22]
plot_output(aeme, model = model)
#> Warning: Using size for a discrete variable is not advised.
#> Warning: Removed 84 rows containing missing values or values outside the scale range
#> (`geom_col()`).
```
