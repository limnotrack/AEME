# Run aquatic model ensemble

Run aquatic model ensemble

## Usage

``` r
run_aeme(
  aeme,
  model,
  return = TRUE,
  ens_n = 1,
  model_controls = NULL,
  nlev = NULL,
  verbose = FALSE,
  debug = FALSE,
  timeout = 0,
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

- return:

  logical; return model output within an \`aeme\` object? Defaults to
  TRUE.

- ens_n:

  numeric; ensemble number to allocate to model output which is loaded.
  Defaults to 1.

- model_controls:

  dataframe; of configuration loaded from "model_controls.csv".

- nlev:

  numeric; number of levels to return in model output. If NULL,
  calculates number of levels based on the \`model_layer_structure\`.

- verbose:

  logical; print model output to console. Defaults to FALSE.

- debug:

  logical; write debug log (Only DYRESM). Defaults to FALSE.

- timeout:

  timeout in seconds, ignored if 0. This is a limit for the elapsed time
  running `command` in a separate process. Fractions of seconds are
  ignored.

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
#> ! Missing values in observed water level
#> ℹ Insufficient water level observations. Using constant water level
#> ℹ Correcting water balance using estimated outflows (method = 2).
#> ℹ Calculating lake level using lake depth and a sinisoidal function.
#> ℹ Building GLM-AED2 for lake wainamu
#> ✔ GLM nml validation completed — no issues detected.
aeme <- run_aeme(aeme = aeme, model = model, path = path)
#> Running models... (Have you tried parallelizing?) [2025-11-11 02:50:56]
#> GLM-AED running... [2025-11-11 02:50:56]
#> GLM-AED run successful! [2025-11-11 02:50:56]
#> Model run complete![2025-11-11 02:50:56]
#> ℹ Retrieving and formatting temp for model glm_aed
#> ℹ Retrieving and formatting salt for model glm_aed
plot_output(aeme, model = model)
#> Warning: Using size for a discrete variable is not advised.
#> Warning: Removed 82 rows containing missing values or values outside the scale range
#> (`geom_col()`).
```
