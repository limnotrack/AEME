# Plot annual water balance components

Calculates and plots annual water balance components including
evaporation, precipitation, inflow, outflow, and net balance for each
model in the Aeme object.

## Usage

``` r
plot_wbal_annual(aeme, model, lake_frac = FALSE, remove_spin_up = FALSE)
```

## Arguments

- aeme:

  Aeme object.

- model:

  character vector; model(s) to plot. If missing, all models in the Aeme
  object will be plotted.

- lake_frac:

  Logical. If TRUE, water balance components are expressed as a fraction
  of lake volume. Default is FALSE.

- remove_spin_up:

  logical; remove spin-up period from plot. Default is TRUE.

## Value

ggplot2 object
