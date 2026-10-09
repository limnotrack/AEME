# Write model configuration from the aeme object

Writes each requested model's configuration files straight from the
`aeme` object's cached state, with no recomputation of any kind – the
hydrodynamic/bgc files come verbatim from `configuration(aeme)`, and
(when `include_boundary = TRUE`) the meteorology/inflow/outflow
boundary-condition files come straight from
`input(aeme)`/`inflows(aeme)`/ `outflows(aeme)`, bypassing
[`build_aeme()`](https://limnotrack.com/reference/build_aeme.md)'s
water-balance/lake-level/ AED-re-derivation pipeline entirely. This
makes it the safe choice for rewriting an already-built (or
[`glm_config_to_aeme()`](https://limnotrack.com/reference/glm_config_to_aeme.md)-loaded)
configuration to disk unchanged – e.g. into a fresh directory – without
the risk of `build_aeme(use_aeme = TRUE)` silently regenerating values
from generic state instead of trusting what's cached.

## Usage

``` r
write_configuration(
  aeme,
  model,
  path = getwd(),
  include_boundary = TRUE,
  apply_params = TRUE
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

  character; path to the directory where the model configuration should
  be written. Default is the current working directory.

- include_boundary:

  logical; also write each model's boundary-condition files
  (meteorology, inflows and outflows, e.g. GLM-AED's
  `bcs/meteo_glm.csv`, `bcs/inflow_*.csv` and `bcs/outflow_*.csv`) in
  the model's own format, straight from `input(aeme)`/`inflows(aeme)`/
  `outflows(aeme)`. Default `TRUE`.

- apply_params:

  logical; apply `parameters(aeme)` when writing. The configuration-file
  parameters are set on the configuration being written, and (for the
  boundary files) the met/inflow/outflow scaling parameters are applied
  to the data being written. Neither `configuration(aeme)` nor
  `input(aeme)`/`inflows(aeme)`/`outflows(aeme)` is changed: the
  parameters only affect what reaches disk. Parameters for a file the
  configuration does not have (e.g. bgc parameters for a model built
  without bgc) are skipped with a warning. Default `TRUE`.

## Value

aeme object which was passed to the function,
