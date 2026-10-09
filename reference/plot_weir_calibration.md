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

  character; model name(s) to plot. Multiple models are overlaid on the
  same panels, coloured by model, rather than faceted – useful for
  comparing evaporation families (e.g. `glm_aed` vs `gotm_wet`) or
  confirming that `dy_cd`/`glm_aed` share a fit. Defaults to every model
  present in `aeme`.

## Value

A patchwork object.
