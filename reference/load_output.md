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

  aeme; object.

- model:

  vector; of models to be used. Can be \`dy_cd\`, \`glm_aed\`,
  \`gotm_wet\`.

- path:

  filepath; where input files are located relative to the current
  working directory.

- model_controls:

  dataframe; of configuration loaded from "model_controls.csv".

- parallel:

  logical; run models in parallel. Defaults to FALSE.

- cl:

  an object of class `"cluster"`.

- ens_n:

  numeric; ensemble number to allocate to model output which is loaded.
  Defaults to 1.

## Value

Updated aeme object with model output
