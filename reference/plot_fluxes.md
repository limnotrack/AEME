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

  Aeme object.

- model:

  character vector; models to use. One or more of `"dy_cd"`,
  `"glm_aed"`, `"gotm_wet"`. Defaults to all models if not found in
  `aeme`.

- cumulative:

  logical; if `TRUE`, plot cumulative fluxes. If `FALSE`, plot
  instantaneous fluxes.

## Value

ggplot2 object
