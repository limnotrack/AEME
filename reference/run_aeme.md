# Run aquatic model ensemble

Run aquatic model ensemble

## Usage

``` r
run_aeme(
  aeme,
  model,
  path,
  return_type = c("aeme", "exec_result", "both", "none"),
  ens_n = 1,
  model_controls = NULL,
  verbose = FALSE,
  debug = FALSE,
  timeout = Inf,
  parallel = FALSE,
  ncores,
  check_output = FALSE
)
```

## Arguments

- aeme:

  aeme; object.

- model:

  vector; of models to be used. Can be `dy_cd`, `glm_aed`, `gotm_wet`.

- path:

  filepath; where input files are located relative to the current
  working directory.

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
  `min(c(detectCores() - 1, length(model)))`.

- check_output:

  logical; check model output after running? Defaults to FALSE.

- return:

  logical; return model output within an `aeme` object? Defaults to
  TRUE.

## Value

an `aeme` object with model output loaded.

## Examples

``` r
aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
aeme <- readRDS(aeme_file)
path <- tempdir()
model_controls <- get_model_controls()
model <- c("glm_aed")
aeme <- build_aeme(path = path, aeme = aeme, model = model,
                   model_controls = model_controls, ext_elev = 5)
#> ℹ All columns already match AEME standard variable names, skipping name
#>   guessing.
#> ℹ All columns already match AEME standard inflow variable names, skipping name
#>   guessing.
#> Warning: ! `SIL_rsi`: SIL_rsi is constant across all rows — this may be a placeholder
#>   value.
#> ℹ Check raw data or unit conversion for this variable.
#> ℹ Using observed water level.
#> ! Missing values in observed water level.
#> ℹ Correcting water balance using estimated outflows (method = 2).
#> ℹ Calculating lake level using lake depth and a sinisoidal function.
#> ℹ Building GLM-AED for lake wainamu
#> ✔ GLM nml validation completed - no issues detected.
if (FALSE) { # \dontrun{
aeme <- run_aeme(aeme)

# Plot model output - temperature by default
plot_output(aeme)
} # }
```
