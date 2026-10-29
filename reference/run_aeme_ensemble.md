# Run an ensemble of parameter sets

Runs a model once per supplied parameter set and stores every member's
output on the returned `Aeme` object as
`output(aeme)$ens_001 ... ens_00k`, alongside `output(aeme)$n_members`.
That is the structure
[`get_var()`](https://limnotrack.com/AEME/reference/get_var.md) reads
with `ens_n`, and
[`write_aeme_parquet()`](https://limnotrack.com/AEME/reference/write_aeme_parquet.md)
exports.

This runs parameter sets you **supply**: it does not sample them.
Drawing sets from parameter bounds and extracting a posterior from a
`pestpp-ies` calibration are provided by the `aemetools` package, which
passes its sets through here. Sets that use zone ratios
([`zone_ratio_param()`](https://limnotrack.com/AEME/reference/zone_ratio_param.md))
work as they are:
[`input_model_parameters()`](https://limnotrack.com/AEME/reference/input_model_parameters.md)
expands them.

Each member is one
[`input_model_parameters()`](https://limnotrack.com/AEME/reference/input_model_parameters.md)
followed by one
[`run_aeme()`](https://limnotrack.com/AEME/reference/run_aeme.md), so a
member's cost is a single model run. A member that errors, times out or
produces no output is dropped with a warning and the survivors are
renumbered.

A model can also fail *without* saying so. GLM, for instance, exits
normally when an unphysical parameter combination drains the lake: the
water level goes to zero or below and the rest of the output is `NA`,
but the run reports success. With `drop_incomplete = TRUE` (the default)
a member is also dropped when its water level (`LKE_lvlwtr`) reaches
zero or is `NA` at any timestep, spin-up included. This needs
`LKE_lvlwtr` among the model's output variables; if it was not written
the check cannot be made and the member is kept.

## Usage

``` r
run_aeme_ensemble(
  aeme,
  model,
  param_sets,
  path,
  parallel = FALSE,
  ncore = NULL,
  model_controls = NULL,
  timeout = Inf,
  drop_incomplete = TRUE,
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

- param_sets:

  the parameter sets to run. Either a list of long `param` data.frames,
  one per member (such as the `aeme_param_sets` object returned by
  `aemetools::pest_posterior_params()`), or a single long data.frame
  with an `ensemble` column identifying each member. Each member needs
  the columns `model`, `file`, `name` and `value`, as in
  [`parameters()`](https://limnotrack.com/AEME/reference/parameters.md)
  (a `group` and `index` are used where present). Names, or the
  `ensemble` ids, are kept as the member labels.

- path:

  character; directory where input files are located. Defaults to the
  path stored in `aeme`, or the current working directory if not set.

- parallel:

  logical; run members in parallel, each worker in its own copy of the
  model directory. Default `FALSE`. The workers load the installed
  version of AEME.

- ncore:

  integer; number of workers when `parallel = TRUE`. `NULL` (default)
  uses one fewer than the cores available, and never more than the
  number of members.

- model_controls:

  data.frame; model configuration, typically loaded via
  [`get_model_controls()`](https://limnotrack.com/AEME/reference/get_model_controls.md).

- timeout:

  Timeout for the process, in seconds, or as a `difftime` object. If it
  is not finished before this, it will be killed.

- drop_incomplete:

  logical; drop members whose water level reached zero or went `NA` (a
  lake that dried out or a model that stopped part-way), as well as
  members that errored. Default `TRUE`. `FALSE` keeps them, `NA`-padded.

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

The `Aeme` object with the ensemble stored in
[`output()`](https://limnotrack.com/AEME/reference/output.md). When
`param_sets` was named, the labels of the surviving members are on
`attr(output(aeme), "realisation")`.

## See also

[`add_param()`](https://limnotrack.com/AEME/reference/add_param.md),
[`input_model_parameters()`](https://limnotrack.com/AEME/reference/input_model_parameters.md),
[`run_aeme()`](https://limnotrack.com/AEME/reference/run_aeme.md),
[`write_aeme_parquet()`](https://limnotrack.com/AEME/reference/write_aeme_parquet.md)

## Examples

``` r
if (FALSE) { # \dontrun{
aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
aeme <- readRDS(aeme_file)
path <- tempdir()
aeme <- build_aeme(path = path, aeme = aeme, model = "glm_aed",
                   ext_elev = 5)

# Ten members, each scattering the default values by up to 10 %
data("aeme_parameters", package = "AEME")
param <- aeme_parameters[aeme_parameters$model == "glm_aed", ]
param_sets <- lapply(1:10, function(i) {
  p <- param
  p$value <- p$value * runif(nrow(p), 0.9, 1.1)
  p
})

aeme <- run_aeme_ensemble(aeme, model = "glm_aed", path = path,
                          param_sets = param_sets)
output(aeme)$n_members
} # }
```
