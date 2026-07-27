# Get AED sed_const2d parameters

Get AED sed_const2d parameters

## Usage

``` r
get_aed_sed_const2d_param(aeme, path, lake_dir = NULL)
```

## Arguments

- aeme:

  Aeme object.

- path:

  character; directory where input files are located. Defaults to the
  path stored in `aeme`, or the current working directory if not set.

- lake_dir:

  Path to the lake AEME directory. If `NULL`, it is derived from
  `aeme`/`path`.

## Value

Data frame with AED sed_const2d parameters
