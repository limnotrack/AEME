# Plot water balance components

Plot water balance components for a given model including evaporation
volume, lake precipitation, lake inflow, and lake outflow.

## Usage

``` r
plot_wbal(aeme, model, cumulative = FALSE)
```

## Arguments

- aeme:

  Aeme object.

- model:

  character vector; model(s) to plot. If missing, all models in the Aeme
  object will be plotted.

- cumulative:

  logical; plot cumulative sum of variable

## Value

ggplot2 object
