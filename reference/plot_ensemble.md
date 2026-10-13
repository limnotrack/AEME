# Plot AEME ensemble output

Plots an ensemble produced by
[`run_aeme_ensemble()`](https://limnotrack.com/AEME/reference/run_aeme_ensemble.md)
as a quantile ribbon (`type = "ribbon"`) or as one line per member
(`type = "line"`), with the matching lake observations overlaid.

`aeme` may be either an `aeme` object carrying `output()$ens_*` or an
[`ensemble_summary()`](https://limnotrack.com/AEME/reference/ensemble_summary.md)
result. Passing a summary skips the per-member extraction, so building
one summary and plotting several variables, depths or intervals from it
is cheap.

## Usage

``` r
plot_ensemble(
  aeme,
  model,
  var_sim = "HYD_temp",
  depth = NULL,
  conf_int = 0.95,
  type = "ribbon",
  remove_spin_up = TRUE,
  add_obs = TRUE,
  var_lims = NULL
)
```

## Arguments

- aeme:

  an `aeme` object with ensemble output, or an
  [`ensemble_summary()`](https://limnotrack.com/AEME/reference/ensemble_summary.md).

- model:

  character; model(s) to plot. Optional when `aeme` is an
  [`ensemble_summary()`](https://limnotrack.com/AEME/reference/ensemble_summary.md).

- var_sim:

  string; of variable to plot

- depth:

  numeric; depth of the variable to extract, referenced to `depth_ref`.
  Default is NULL. If NULL, the variable profiles are extracted.

- conf_int:

  numeric; central interval for the ribbon. Default 0.95. When `aeme` is
  a summary the interval's quantiles must have been included in its
  `probs`.

- type:

  character; `"ribbon"` (default) or `"line"`.

- remove_spin_up:

  logical; remove spin-up period from plot. Default is TRUE.

- add_obs:

  logical; add observations to plot

- var_lims:

  numeric vector of length 2; limits for the variable. Defaults to NULL
  and will generate common limits for all variables.

## Value

ggplot object

## See also

[`ensemble_summary()`](https://limnotrack.com/AEME/reference/ensemble_summary.md),
[`run_aeme_ensemble()`](https://limnotrack.com/AEME/reference/run_aeme_ensemble.md)
