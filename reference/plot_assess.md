# Plot model performance metrics

Visualise the model performance statistics returned by
[`assess_aeme`](https://limnotrack.com/reference/assess_aeme.md). Four
complementary views are available via `type`:

- `"dot"` - Cleveland dot plot, one facet per metric with a free x-axis,
  points coloured by model. The general-purpose default: it keeps
  metrics on their own scales so native-unit errors (bias, rmse, nmae)
  and bounded skill scores (nse, kge, d2, r) are not forced onto a
  common axis.

- `"heatmap"` - Tile plot of model (x) against variable (y), faceted by
  metric. Fill is the value rescaled to `0-1` within each
  metric/variable and oriented so that `1` is the best-performing model;
  the raw value is printed in each tile.

- `"taylor"` - Normalised Taylor diagram (Taylor, 2001), one point per
  model per variable, encoding the correlation and the ratio of modelled
  to observed standard deviation. The observations plot at `(1, 0)`.

- `"target"` - Target diagram (Jolliff et al., 2009), plotting
  normalised bias against normalised unbiased RMSD. Points inside the
  unit circle have a total RMSD smaller than the observed standard
  deviation.

## Usage

``` r
plot_assess(
  aeme,
  model,
  var_sim,
  type = c("dot", "heatmap", "taylor", "target"),
  metrics = c("bias", "rmse", "nmae", "nse", "kge", "d2", "r")
)
```

## Arguments

- aeme:

  Aeme object.

- model:

  character vector; models to use. One or more of `"dy_cd"`,
  `"glm_aed"`, `"gotm_wet"`, `"simstrat_aed2"`. Defaults to all models
  if not found in `aeme`.

- var_sim:

  string; of variable to plot

- type:

  character; which view to draw. One of `"dot"` (default), `"heatmap"`,
  `"taylor"` or `"target"`.

- metrics:

  character vector; metrics to include in the `"dot"` and `"heatmap"`
  views. Defaults to
  `c("bias", "rmse", "nmae", "nse", "kge", "d2", "r")`. Ignored for
  `"taylor"` and `"target"`.

## Value

A ggplot2 object.

## Details

`"dot"` and `"heatmap"` are built from
[`assess_aeme`](https://limnotrack.com/reference/assess_aeme.md) output;
`"taylor"` and `"target"` are computed from the paired observed/modelled
values returned by
[`get_var`](https://limnotrack.com/reference/get_var.md).

## See also

[`assess_aeme`](https://limnotrack.com/reference/assess_aeme.md) for the
underlying statistics.

## Examples

``` r
if (FALSE) { # \dontrun{
  metrics <- assess_aeme(aeme = aeme, model = c("glm_aed", "gotm_wet"))
  plot_assess(aeme = aeme, model = c("glm_aed", "gotm_wet"))
  plot_assess(aeme = aeme, type = "heatmap")
  plot_assess(aeme = aeme, type = "taylor", var_sim = "HYD_temp")
} # }
```
