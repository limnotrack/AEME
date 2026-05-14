# Get number of sediment zones in GLM-AED model

Get number of sediment zones in GLM-AED model

## Usage

``` r
get_glm_sed_params(aeme, path, lake_dir = NULL)
```

## Arguments

- aeme:

  aeme; object.

- path:

  filepath; where input files are located relative to the current
  working directory.

- lake_dir:

  Path to the lake AEME directory. If NULL, it will be computed from
  `aeme` and `path`.

## Value

Number of sediment zones
