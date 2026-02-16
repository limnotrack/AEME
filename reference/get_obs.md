# Get observations for a given variable from an Aeme object

Get observations for a given variable from an Aeme object

## Usage

``` r
get_obs(aeme, var_sim, depth_range = NULL, time_filter = FALSE)
```

## Arguments

- aeme:

  aeme; object.

- var_sim:

  character; variable in the AEME format (e.g. "HYD_temp"). Can be a
  vector. If missing, all variables are returned.

- depth_range:

  numeric vector of length 2; depth range (in meters) to filter
  observations. If NULL, all depths are returned.

- time_filter:

  logical; if TRUE, filter observations to the time range of the Aeme
  object. If FALSE, all observations are returned regardless of time.

## Value

A data frame with the following columns:

- `Date`: Date of observation

- `var_aeme`: Name of the variable in the AEME format

- `depth_from`: Depth from which the variable is extracted

- `depth_to`: Depth to which the variable is extracted

- `value`: Value of the variable
