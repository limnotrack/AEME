# Plot lake water level

Plot lake water level

## Usage

``` r
plot_wlev(aeme, model, facet = FALSE, ...)
```

## Arguments

- aeme:

  Aeme object.

- model:

  character vector; model(s) to plot. If missing, all models in the Aeme
  object will be plotted.

- facet:

  logical; if `TRUE`, for variables with depth, plot each model in a
  separate facet. If `FALSE`, plot each model in a separate plot and
  return a list of plots. If `TRUE`, for variables without depth, plot
  each model in a separate facet. If `FALSE`, plot each model as a
  separate line and return a plot. This only applies to variables
  without a depth component.

- ...:

  Additional arguments passed to `plot_output`

## Value

A ggplot object
