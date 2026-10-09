# Show the observation variables present in an Aeme object

Summarises the observations stored in an Aeme object (both the lake
profile observations and the water level observations), reporting one
row per observed variable along with how much data is available for it.

## Usage

``` r
get_obs_vars(aeme, time_filter = FALSE)
```

## Arguments

- aeme:

  Aeme object.

- time_filter:

  logical; if TRUE, only observations within the simulation period (see
  [`time()`](https://limnotrack.com/reference/time.md)) are considered.
  Default is FALSE.

## Value

A data frame with one row per observation variable and the columns:

- `var_aeme`: variable name in the AEME format

- `name_text`: display name of the variable (from `key_naming`)

- `source`: which observation slot the variable comes from (`"lake"` or
  `"level"`)

- `n`: number of observations

- `n_dates`: number of unique dates

- `n_depths`: number of unique depths (NA for water level)

- `date_start`, `date_stop`: first and last observation date

Returns `NULL` if the Aeme object contains no observations.

## See also

[`list_obs_vars()`](https://limnotrack.com/reference/list_obs_vars.md)
for a named vector of the variable names,
[`get_mod_obs_vars()`](https://limnotrack.com/reference/get_mod_obs_vars.md)
for the variables shared with model output.

## Examples

``` r
aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
aeme <- readRDS(aeme_file)
get_obs_vars(aeme)
#> # A tibble: 13 × 8
#>    var_aeme   name_text      source     n n_dates n_depths date_start date_stop 
#>    <chr>      <chr>          <chr>  <int>   <int>    <int> <date>     <date>    
#>  1 CAR_doc    Dissolved org… lake      18      18        4 2019-08-07 2021-06-10
#>  2 CHM_oxy    Dissolved oxy… lake     200      16       13 2019-10-29 2021-06-10
#>  3 CHM_salt   Salinity       lake     211      17       13 2019-08-07 2021-06-10
#>  4 HYD_temp   Water tempera… lake     224      18       13 2019-08-07 2021-06-10
#>  5 NIT_amm    Ammoniacal ni… lake      36      18        7 2019-08-07 2021-06-10
#>  6 NIT_nit    Nitrate        lake      36      18        7 2019-08-07 2021-06-10
#>  7 NIT_tn     Total nitrogen lake      36      18        7 2019-08-07 2021-06-10
#>  8 PHS_frp    Phosphate      lake      36      18        7 2019-08-07 2021-06-10
#>  9 PHS_tp     Total phospho… lake      36      18        7 2019-08-07 2021-06-10
#> 10 PHY_cyano  Cyanobacteria  lake      16      16        1 2019-08-07 2021-06-10
#> 11 PHY_tchla  Total chlorop… lake      18      18        4 2019-08-07 2021-06-10
#> 12 RAD_secchi Secchi disk d… lake      14      14        1 2019-08-07 2021-06-10
#> 13 LKE_lvlwtr Water level    level      9       9       NA 2020-07-06 2021-06-10
```
