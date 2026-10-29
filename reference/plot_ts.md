# Plot multi-variable timeseries

Plot multi-variable timeseries

## Usage

``` r
plot_ts(
  aeme,
  model,
  var_sim,
  remove_spin_up = TRUE,
  add_obs = TRUE,
  depth_range = NULL,
  ens_n = 1,
  obs_size = 2.5
)
```

## Arguments

- aeme:

  Aeme object.

- model:

  character vector; model(s) to plot. If missing, all models in the Aeme
  object will be plotted.

- var_sim:

  character; variable in the AEME format (e.g. "HYD_temp"). Can be a
  vector.

- remove_spin_up:

  logical; remove spin-up period from plot. Default is TRUE.

- add_obs:

  logical; add observations to the plot. Default is TRUE.

- depth_range:

  numeric; range of depths to average. Default is NULL, which averages
  over all depths.

- ens_n:

  integer; ensemble number to plot. Default is 1.

- obs_size:

  numeric; size of the observation points. Default is 2.5. A single
  variable is shown with black points (white outline); multiple
  variables are filled to match the line colour (white outline).

## Value

A ggplot object
