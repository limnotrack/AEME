# Align observation depth data with model data

Align observation depth data with model data

## Usage

``` r
align_depth_data(aeme, model, var_sim, ens_n = 1, return_df = TRUE)
```

## Arguments

- aeme:

  Aeme object.

- model:

  character vector; models to use. One or more of `"dy_cd"`,
  `"glm_aed"`, `"gotm_wet"`, `"simstrat_aed2"`. Defaults to all models
  if not found in `aeme`.

- var_sim:

  character; variable in the AEME format (e.g. "HYD_temp").

- ens_n:

  integer; ensemble number to plot. Default is 1.

- return_df:

  logical; if TRUE, return a dataframe; if FALSE, return a list. Default
  is TRUE.

## Value

A data frame with the following columns:

- `Date`: Date of observation

- `depth`: Depth of observation

- `elev`: Elevation of observation

- `Model`: Model name

- `var_sim`: Variable name

- `value`: Value of the variable

- `depth_from`: Depth from which the variable is extracted

- `depth_to`: Depth to which the variable is extracted
