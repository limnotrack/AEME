# Check if water temperature profile is stratified

Check if water temperature profile is stratified

## Usage

``` r
is_strat(wtr, depths, t_diff = 1)
```

## Arguments

- wtr:

  numeric vector; water temperature profile

- depths:

  numeric vector; depths corresponding to water temperature profile

- t_diff:

  numeric; minimum temperature difference between surface and bottom to
  consider the profile stratified. Default is 1 degree Celsius.

## Value

logical; TRUE if profile is stratified, FALSE otherwise
