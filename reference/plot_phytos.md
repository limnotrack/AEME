# Plot phytoplankton variables

Plot phytoplankton variables

## Usage

``` r
plot_phytos(
  aeme,
  model,
  add_obs = TRUE,
  remove_spin_up = TRUE,
  depth_range = NULL,
  ens_n = 1
)
```

## Arguments

- aeme:

  Aeme object.

- model:

  character vector; model(s) to plot. If missing, all models in the Aeme
  object will be plotted.

- add_obs:

  logical; add observations to plot

- remove_spin_up:

  logical; remove spin-up period from plot. Default is TRUE.

- depth_range:

  numeric; range of depths to plot. Default is NULL, which averages over
  all depths.

- ens_n:

  integer; ensemble number to plot. Default is 1.

## Value

A ggplot object
