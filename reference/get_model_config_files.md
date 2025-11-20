# Get model configuration files paths

Get model configuration files paths

## Usage

``` r
get_model_config_files(aeme, model, path, lake_dir = NULL)
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

- lake_dir:

  Path to the lake AEME directory. If NULL, it will be computed from
  \`aeme\` and \`path\`.

## Value

A list with model configuration files paths
