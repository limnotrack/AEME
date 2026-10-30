# Run aquatic model ensemble

Run aquatic model ensemble

## Usage

``` r
run_aeme(
  aeme,
  model,
  path,
  args = character(),
  return_type = c("aeme", "exec_result", "both", "none"),
  ens_n = 1,
  model_controls = NULL,
  verbose = getOption("AEME.inform", FALSE),
  debug = FALSE,
  timeout = Inf,
  parallel = FALSE,
  ncores,
  check_output = FALSE,
  keep_diag = TRUE,
  extra_vars = "none",
  sed_layers = NULL,
  zone_summary = FALSE
)
```

## Arguments

- aeme:

  Aeme object.

- model:

  character vector; models to use. One or more of `"dy_cd"`,
  `"glm_aed"`, `"gotm_wet"`, `"simstrat_aed2"`. Defaults to all models
  if not found in `aeme`.

- path:

  character; directory where input files are located. Defaults to the
  path stored in `aeme`, or the current working directory if not set.

- args:

  Character vector, arguments to the command.

- return_type:

  character; one of `"aeme"` (default), `"exec_result"`, `"both"`, or
  `"none"`. `"aeme"` returns the `aeme` object with model output loaded;
  `"exec_result"` returns the raw
  [`processx::run()`](http://processx.r-lib.org/reference/run.md)
  result(s) for each model; `"both"` returns a list with both `aeme` and
  `exec_result` elements; `"none"` returns `NULL` invisibly (useful when
  only the model run's side effects, i.e. the output files, are wanted).

- ens_n:

  numeric; ensemble number to allocate to model output which is loaded.
  Defaults to 1. A run with `ens_n = 1` clears any existing output; a
  run with another number adds (or replaces) that member and keeps the
  rest.

- model_controls:

  data.frame; model configuration, typically loaded via
  [`get_model_controls()`](https://limnotrack.com/AEME/reference/get_model_controls.md).

- verbose:

  logical; print model output to console. Defaults to
  `getOption("AEME.inform", FALSE)`.

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

- keep_diag:

  logical; keep the `diag` (diagnostic/scalar) and `sediment` sub-lists
  of each model's output (variables with dimensions other than time or
  depth x time, keyed by raw model names). Default `TRUE`, which keeps
  whatever was requested through `extra_vars` (nothing is requested by
  default, so by default there is nothing to keep). Set `FALSE` to drop
  the sub-lists even if they were requested.

- extra_vars:

  character; for GLM-AED, which sediment, flux and diagnostic variables
  beyond the standard set to read from `output.nc`. Default `"none"`:
  only the standard variables (`vars_sim` and the lake fluxes) are read,
  so nothing extra is loaded or stored unless asked for. Otherwise AEME
  names from
  [glm_extra_vars](https://limnotrack.com/AEME/reference/glm_extra_vars.md)
  (e.g. `"SED_temp"`), raw netCDF names, the keywords `"sediment"` /
  `"diag"` (every variable in that sub-list), or `"all"` for every
  variable in the file (this was the behaviour before extra output had
  to be requested). Unrequested variables are never read, which keeps
  loading fast and ensemble output small. See
  [`read_glm_output()`](https://limnotrack.com/AEME/reference/read_glm_output.md).

- sed_layers:

  integer; GLM-AED sediment layers to keep for variables with a
  sediment-layer dimension (`SED_temp`). `NULL` keeps all. See
  [`read_glm_output()`](https://limnotrack.com/AEME/reference/read_glm_output.md).

- zone_summary:

  logical; for GLM-AED, also add an area-weighted lake-wide series for
  each requested sediment-zone variable. See
  [`read_glm_output()`](https://limnotrack.com/AEME/reference/read_glm_output.md).
  Default `FALSE`.

## Value

Depends on `return_type` – an `aeme` object with model output loaded
(`"aeme"`), the raw
[`processx::run()`](http://processx.r-lib.org/reference/run.md)
result(s) (`"exec_result"`), a list with both (`"both"`), or `NULL`
invisibly (`"none"`).

## Examples

``` r
aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
aeme <- readRDS(aeme_file)
path <- tempdir()
model_controls <- get_model_controls()
model <- c("glm_aed")
aeme <- build_aeme(path = path, aeme = aeme, model = model,
                   model_controls = model_controls, ext_elev = 5)
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
#> ✔ Estimating surface water temperature [26ms]
#> 
#> Estimating lake water levels for glm_aed
#>   ℹ Optimizing parameters for water balance
#>   ✔ Optimization Complete: C = 0.3343, h_inv = 23.4915, Final RMSE = 0.1431
#> ℹ Correcting water balance using estimated outflows (method = 2).
#> 
#> ── Building GLM-AED for lake wainamu ──
#> 
#> ! Fitted weir crest (23.49 m) is below the initial lake level; using 23.64 m.
#> ✔ GLM nml validation completed - no issues detected.
if (FALSE) { # \dontrun{
aeme <- run_aeme(aeme)

# Plot model output - temperature by default
plot_output(aeme)
} # }
```
