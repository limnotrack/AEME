# Visualise calibrated weir parameters (C, h_inv) from calc_water_balance().

Visualise calibrated weir parameters (C, h_inv) from
calc_water_balance().

## Usage

``` r
plot_weir_calibration(aeme, model)
```

## Arguments

- aeme:

  Aeme object.

- model:

  character vector; models to use. One or more of `"dy_cd"`,
  `"glm_aed"`, `"gotm_wet"`, `"simstrat_aed2"`. Defaults to all models
  if not found in `aeme`.

## Value

A patchwork object.
