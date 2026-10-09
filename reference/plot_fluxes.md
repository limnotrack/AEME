# Plot fluxes

Plot heat fluxes from AEME simulations. This includes incoming shortwave
radiation, net longwave radiation, evaporative heat flux, and sensible
heat flux.

## Usage

``` r
plot_fluxes(aeme, model, facet_by = c("flux", "model"), ...)
```

## Arguments

- aeme:

  Aeme object.

- model:

  character vector; models to use. One or more of `"dy_cd"`,
  `"glm_aed"`, `"gotm_wet"`, `"simstrat_aed2"`. Defaults to all models
  if not found in `aeme`.

- facet_by:

  character; either `"flux"` or `"model"`. If `"flux"`, create a
  separate facet for each flux. If `"model"`, create a separate facet
  for each model.

- ...:

  additional arguments passed to
  [`get_var`](https://limnotrack.com/reference/get_var.md)

## Value

ggplot2 object
