# Get model output file

Get model output file

## Usage

``` r
get_model_outfile(aeme = NULL, model, path = NULL, lake_dir = NULL)
```

## Arguments

- aeme:

  Aeme object.

- model:

  character vector; models to use. One or more of `"dy_cd"`,
  `"glm_aed"`, `"gotm_wet"`. Defaults to all models if not found in
  `aeme`.

- path:

  character; directory where input files are located. Defaults to the
  path stored in `aeme`, or the current working directory if not set.

- lake_dir:

  Path to the lake AEME directory. If NULL, it will be computed from
  `aeme` and `path`.

## Value

list of model output files.
