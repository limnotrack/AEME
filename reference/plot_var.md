# Plot AEME variable

Plot AEME variable

## Usage

``` r
plot_var(
  df = NULL,
  aeme,
  model,
  var_sim,
  ylim = NULL,
  xlim,
  var_lims = NULL,
  obs = NULL,
  add_obs = TRUE,
  point_size = 2,
  level = FALSE,
  facet = FALSE,
  cumulative = FALSE,
  print_plots = FALSE
)
```

## Arguments

- df:

  dataframe; output from [`get_var`](get_var.md). If `NULL`,
  [`get_var`](get_var.md) will be called and `var_sim` will be used to
  extract the variable of interest from `aeme` for each model.

- aeme:

  aeme; object.

- model:

  character vector; model(s) to plot. If missing, all models in the Aeme
  object will be plotted.

- var_sim:

  string; of variable to plot

- ylim:

  numeric vector of length 2; limits for the y-axis. Defaults to NULL
  and calculates this based on the data to be plotted.

- xlim:

  numeric; x-axis limits

- var_lims:

  numeric vector of length 2; limits for the variable. Defaults to NULL
  and will generate common limits for all variables.

- obs:

  list; output from [`observations`](observations.md)

- add_obs:

  logical; add observations to plot

- level:

  logical; include lake level. Only applies for contour plots.

- facet:

  logical; if `TRUE`, for variables with depth, plot each model in a
  separate facet. If `FALSE`, plot each model in a separate plot and
  return a list of plots. If `TRUE`, for variables without depth, plot
  each model in a separate facet. If `FALSE`, plot each model as a
  separate line and return a plot. This only applies to variables
  without a depth component.

- cumulative:

  logical; plot cumulative sum of variable

- print_plots:

  logical; print plots

## Value

ggplot2 object or list of ggplot2 objects
