# Load AEME output to the aeme object

Reads the output of models that have already been run and, by default,
stores it in the `output` slot of the `Aeme` object. With
`return_type = "output"` the output is returned instead and the `Aeme`
object is left as it was, which is useful for looking at extra output
(see `extra_vars`) without storing it alongside the output already on
the object.

## Usage

``` r
load_output(
  aeme,
  model,
  path = NULL,
  lake_dir = NULL,
  model_controls,
  parallel = FALSE,
  cl = NULL,
  ens_n = 1,
  keep_diag = TRUE,
  extra_vars = "none",
  sed_layers = NULL,
  zone_summary = FALSE,
  return_type = c("aeme", "output")
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

- lake_dir:

  Path to the lake AEME directory. If `NULL`, it is derived from
  `aeme`/`path`.

- model_controls:

  data.frame; model configuration, typically loaded via
  [`get_model_controls()`](https://limnotrack.com/AEME/reference/get_model_controls.md).

- parallel:

  logical; run models in parallel. Defaults to FALSE.

- cl:

  an object of class `"cluster"`.

- ens_n:

  numeric; ensemble number to allocate to model output which is loaded.
  Defaults to 1. A run with `ens_n = 1` clears any existing output; a
  run with another number adds (or replaces) that member and keeps the
  rest.

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

- return_type:

  character; `"aeme"` (default) returns the `Aeme` object with the
  output stored in it. `"output"` returns the output without storing it:
  a list named by model, with one `aeme_output` (see
  [`is_aeme_output()`](https://limnotrack.com/AEME/reference/is_aeme_output.md))
  per model, read in the same way (simulation window, daily means, unit
  conversions) as the output that is stored. Because nothing is stored,
  `ens_n` is ignored. The result can be given to functions that take a
  model output directly, such as
  [`plot_model_output()`](https://limnotrack.com/AEME/reference/plot_model_output.md),
  but not to those that need an `Aeme` object, such as
  [`get_var()`](https://limnotrack.com/AEME/reference/get_var.md).

## Value

With `return_type = "aeme"`, the updated `Aeme` object with model
output. With `return_type = "output"`, a named list of `aeme_output`
objects, one per model.
