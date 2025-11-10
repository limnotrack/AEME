# Plot phytoplankton variables

Plot phytoplankton variables

## Usage

``` r
plot_phs(
  aeme,
  model,
  add_obs = TRUE,
  depth_range = NULL,
  remove_spin_up = TRUE,
  ens_n = 1
)
```

## Arguments

- aeme:

  aeme; object.

- model:

  character vector; model(s) to plot. If missing, all models in the Aeme
  object will be plotted.

- add_obs:

  logical; add observations to plot

- depth_range:

  numeric; range of depths to plot. Default is NULL, which averages over
  all depths.

- remove_spin_up:

  logical; remove spin-up period from plot. Default is TRUE.

- ens_n:

  integer; ensemble number to plot. Default is 1.

## Value

A ggplot object
