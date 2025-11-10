# Load AEME output to the aeme object

Load AEME output to the aeme object

## Usage

``` r
load_output(
  model,
  aeme,
  path,
  model_controls,
  parallel = FALSE,
  cl = NULL,
  nlev = NULL,
  ens_n = 1
)
```

## Arguments

- model:

  vector; of models to be used. Can be \`dy_cd\`, \`glm_aed\`,
  \`gotm_wet\`.

- aeme:

  aeme; object.

- path:

  filepath; where input files are located relative to the current
  working directory.

- model_controls:

  dataframe; of configuration loaded from "model_controls.csv".

- parallel:

  logical; run models in parallel. Defaults to FALSE.

- cl:

  an object of class `"cluster"`.

- nlev:

  numeric; number of levels to return in model output. If NULL,
  calculates number of levels based on the \`model_layer_structure\`.

- ens_n:

  numeric; ensemble number to allocate to model output which is loaded.
  Defaults to 1.

## Value

Updated aeme object with model output
