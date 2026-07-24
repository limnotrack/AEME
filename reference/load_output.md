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
  ens_n = 1
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

- model_controls:

  data.frame; model configuration, typically loaded via
  [`get_model_controls()`](https://limnotrack.com/reference/get_model_controls.md).

- parallel:

  logical; run models in parallel. Defaults to FALSE.

- cl:

  an object of class `"cluster"`.

- ens_n:

  numeric; ensemble number to allocate to model output which is loaded.
  Defaults to 1.

## Value

Updated aeme object with model output
