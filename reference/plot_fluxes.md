# Plot fluxes

Plot heat fluxes from AEME simulations. This includes incoming shortwave
radiation, net longwave radiation, evaporative heat flux, and sensible
heat flux.

## Usage

``` r
plot_fluxes(aeme, model, cumulative = FALSE)
```

## Arguments

- aeme:

  aeme; object.

- model:

  vector; of models to be used. Can be `dy_cd`, `glm_aed`, `gotm_wet`.

- cumulative:

  logical; if `TRUE`, plot cumulative fluxes. If `FALSE`, plot
  instantaneous fluxes.

## Value

ggplot2 object
