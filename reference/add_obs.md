# Add observations to Aeme object

Add observations to Aeme object

## Usage

``` r
add_obs(aeme, lake = NULL, level = NULL)
```

## Arguments

- aeme:

  Aeme object.

- lake:

  data frame with columns "Date", "var_aeme", "depth_from", "depth_to"
  and "value". If NULL, no observations are added.

- level:

  data frame with columns "Date", "var_aeme" and "value". If NULL, no
  observations are added.

## Value

Aeme object with observations added
