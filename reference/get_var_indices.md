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
  depth_range = NULL,
  use_obs = TRUE
)
```

## Arguments

- nc:

  An object of class `ncdf4` (as returned by either function
  [`nc_open`](https://rdrr.io/pkg/ncdf4/man/nc_open.html) or function
  [`nc_create`](https://rdrr.io/pkg/ncdf4/man/nc_create.html).

- model:

  character vector; models to use. One or more of `"dy_cd"`,
  `"glm_aed"`, `"gotm_wet"`. Defaults to all models if not found in
  `aeme`.

- aeme:

  Aeme object.

- path:

  character; directory where input files are located. Defaults to the
  path stored in `aeme`, or the current working directory if not set.

- vars_sim:

  character; vector of AEME variable names to get indices for.

- month:

  numeric; vector of months to subset the data.

- depth_range:

  numeric; vector of depth ranges, length two to subset the data.

- use_obs:

  logical; if TRUE, use the observation months and depth ranges from the
  AEME object.

## Value

list; of variable indices. Each list element corresponds to a variable
in vars_sim and contains a list with time indices, depth values, and
dates. Time indices correspond to the positions in the model output time
series that match the Date but are the corresponding index in the model
output.
