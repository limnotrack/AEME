# Get variable indices

Get variable indices for an AEME variable.

## Usage

``` r
get_var_indices(
  nc = NULL,
  model,
  aeme,
  path,
  vars_sim,
  month = NULL,
  depth_range = NULL
)
```

## Arguments

- nc:

  An object of class `ncdf4` (as returned by either function
  [`nc_open`](https://rdrr.io/pkg/ncdf4/man/nc_open.html) or function
  [`nc_create`](https://rdrr.io/pkg/ncdf4/man/nc_create.html).

- model:

  vector; of models to be used. Can be \`dy_cd\`, \`glm_aed\`,
  \`gotm_wet\`.

- aeme:

  aeme; object.

- path:

  filepath; where input files are located relative to the current
  working directory.

- vars_sim:

  character; vector of AEME variable names to get indices for.

- month:

  numeric; vector of months to subset the data.

- depth_range:

  numeric; vector of depth ranges, length two to subset the data.

## Value

list; of variable indices.
