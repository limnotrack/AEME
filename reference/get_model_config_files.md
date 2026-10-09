# Get model configuration files paths

Get model configuration files paths

## Usage

``` r
get_model_config_files(aeme = NULL, model, path = NULL, lake_dir)
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

  **\[deprecated\]** Use `path` instead of `lake_dir`

## Value

A list with model configuration files paths
