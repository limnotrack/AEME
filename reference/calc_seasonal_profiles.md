# Calculate seasonal profiles

Calculate seasonal profiles

## Usage

``` r
calc_seasonal_profiles(aeme, model, ens_n = 1)
```

## Arguments

- aeme:

  Aeme object.

- model:

  character vector; models to use. One or more of `"dy_cd"`,
  `"glm_aed"`, `"gotm_wet"`, `"simstrat_aed2"`. Defaults to all models
  if not found in `aeme`.

## Value

Data frame with seasonal profiles for each model and variable
