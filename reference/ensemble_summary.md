# Summarise an AEME ensemble across members

[`run_aeme_ensemble()`](https://limnotrack.com/AEME/reference/run_aeme_ensemble.md)
stores every member's full model output on the `aeme` object. Turning
that into a plot or a skill score means, each time, looping over the
members and pulling the variable out with
[`get_var()`](https://limnotrack.com/AEME/reference/get_var.md) - the
expensive step. `ensemble_summary()` does that extraction **once** and
returns a tidy object holding

- `stats` - one row per date (and depth, and model, and variable) with
  the ensemble `mean`, `sd`, `n` and the quantiles named in `probs`;

- `members` - the extracted long frame the statistics were built from
  (`Date`, `depth`, `value`, `Model`, `var_sim`, `ens`), kept unless
  `keep_members = FALSE`;

- `obs` - the matching lake observations, depth-aligned to `depths`.

[`plot_ensemble()`](https://limnotrack.com/AEME/reference/plot_ensemble.md)
accepts the result directly, so a summary built once can be replotted
for several variables, depths and intervals for free.

[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) returns
the `stats` table - the long per-date summary, ready to write out or
join to something else.

## Usage

``` r
ensemble_summary(
  aeme,
  model,
  vars_sim = "HYD_temp",
  depths = NULL,
  probs = c(0.025, 0.25, 0.5, 0.75, 0.975),
  remove_spin_up = TRUE,
  add_obs = TRUE,
  depth_ref = c("surface", "bottom"),
  obs_tol = 0.5,
  keep_members = TRUE
)

# S3 method for class 'aeme_ensemble_summary'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- aeme:

  an `aeme` object with ensemble output, or an `ensemble_summary()`.

- model:

  character; model(s) to plot. Optional when `aeme` is an
  `ensemble_summary()`.

- vars_sim:

  character; one or more AEME variables to summarise.

- depths:

  numeric; depths (m, referenced to `depth_ref`) at which to summarise
  depth-resolved variables. `NULL` (default) pools all layers for a
  depth-resolved variable - matching the historical
  [`plot_ensemble()`](https://limnotrack.com/AEME/reference/plot_ensemble.md)
  behaviour - and is the only sensible value for a 1-D variable such as
  `LKE_lvlwtr` or `HYD_thmcln`.

- probs:

  numeric; quantile probabilities for the summary bands. `0.5` is always
  added so a median is available.

- remove_spin_up:

  logical; remove spin-up period from plot. Default is TRUE.

- add_obs:

  logical; add observations to plot

- depth_ref:

  character; `"surface"` (default) or `"bottom"`, passed to
  [`get_var()`](https://limnotrack.com/AEME/reference/get_var.md):
  whether `depths` are metres below the surface or metres above the lake
  bed (see
  [`get_var()`](https://limnotrack.com/AEME/reference/get_var.md) for
  how the bottom follows the water level). Observations are placed in
  the same reference before they are matched to `depths`. Observations
  recorded below the surface are the usual case; those recorded as a
  height above the bed (see
  [`add_obs()`](https://limnotrack.com/AEME/reference/add_obs.md)) are
  used as they are for `"bottom"`. An observation in the other reference
  is converted with the height of the water surface above the bed on its
  date: from the lake level observations, interpolated in time, or where
  there are none the ensemble median of the modelled level.

- obs_tol:

  numeric; half-width (m) of the window used to attach observations to
  each requested depth. Default 0.5.

- keep_members:

  logical; keep the extracted per-member frame on the result. Default
  `TRUE`. `FALSE` drops it - smaller object, but
  `plot_ensemble(type = "line")` and future member-level scores then
  can't be computed from it.

- x:

  an `aeme_ensemble_summary`.

- row.names, optional:

  passed to the data.frame method; ignored.

- ...:

  unused.

## Value

An `aeme_ensemble_summary` object: a list of `stats`, `members` (or
`NULL`) and `obs` (or `NULL`), with `probs`, `depths`, `depth_ref`,
`vars_sim`, `vars_2d`, `n_members` and `period` attributes.

## See also

[`plot_ensemble()`](https://limnotrack.com/AEME/reference/plot_ensemble.md),
[`run_aeme_ensemble()`](https://limnotrack.com/AEME/reference/run_aeme_ensemble.md)
