# Get column names for the observational data frame

Get column names for the observational data frame

## Usage

``` r
get_obs_column_names(include_optional = FALSE)
```

## Arguments

- include_optional:

  logical; if `TRUE`, append the optional columns (`depth_to`, `sd`,
  `depth_ref`, `height_above_bed`) after the required columns. Default
  `FALSE`.

## Value

Character vector of column names for observational data. The required
columns are `Date` (a UTC `POSIXct`; daily observations anchored at
12:00:00), `var_aeme`, `depth` and `value`. The optional columns are
`depth_to` (bottom of an integrated sample), `sd` (measurement standard
deviation, in the variable's units), `depth_ref` (`"surface"` or
`"bottom"`: how the depth was recorded) and `height_above_bed` (metres
above the lake bed, for observations recorded relative to the bottom;
`depth` is always metres below the surface).
