# Load model configuration to the aeme object

Load model configuration to the aeme object

## Usage

``` r
load_configuration(aeme, model, model_controls = NULL, use_bgc = FALSE, path)
```

## Arguments

- aeme:

  aeme; object.

- model:

  vector; of models to be used. Can be \`dy_cd\`, \`glm_aed\`,
  \`gotm_wet\`.

- model_controls:

  dataframe; of configuration loaded from "model_controls.csv".

- use_bgc:

  logical; switch to use the biogeochemical model.

- path:

  filepath; where input files are located relative to the current
  working directory.

## Value

Updated aeme object with model configuration
