# Load AEME output to the aeme object

Load AEME output to the aeme object

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
  extra_vars = NULL,
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
  Defaults to 1.

- keep_diag:

  logical; keep the `diag` (diagnostic/scalar) and `sediment` sub-lists
  of each model's output (variables with dimensions other than time or
  depth x time, keyed by raw model names). Default `TRUE`; set to
  `FALSE` to save memory.

- extra_vars:

  character; for GLM-AED, which sediment, flux and diagnostic variables
  beyond the standard set to read from `output.nc`. `NULL` (default)
  reads all of them. `"none"` reads none. Otherwise AEME names from
  [glm_extra_vars](https://limnotrack.com/AEME/reference/glm_extra_vars.md)
  (e.g. `"SED_temp"`), raw netCDF names, and/or the keywords
  `"sediment"` / `"diag"`. Unrequested variables are never read, which
  keeps ensemble output small. See
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

Updated aeme object with model output
