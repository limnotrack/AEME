# Get column names for the observational data frame

Get column names for the observational data frame

## Usage

``` r
get_obs_column_names(include_optional = FALSE)
```

## Arguments

- include_optional:

  logical; if `TRUE`, append the optional columns (`depth_to`, `sd`)
  after the required columns. Default `FALSE`.

## Value

Character vector of column names for observational data. The required
columns are `Date` (a UTC `POSIXct`; daily observations anchored at
12:00:00), `var_aeme`, `depth` and `value`. The optional columns are
`depth_to` (bottom of an integrated sample) and `sd` (measurement
standard deviation, in the variable's units).
