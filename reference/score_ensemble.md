# Skill scores for an AEME ensemble

Verifies an ensemble against the lake observations. For every
observation that falls on a date (and depth) the ensemble covers, the
members' values at that point are compared with the observed value, and
the comparisons are summarised per model, variable and depth, and over
all depths.

The scores are

- `coverage` - fraction of observations inside the central `conf_int`
  interval of the members. A reliable ensemble covers about `conf_int`;
  much less means it is over-confident. With few members the interval is
  estimated from empirical quantiles and under-covers even for a
  reliable ensemble: an `n`-member ensemble covers about
  `(n - 1) * conf_int / (n + 1)`, roughly 80 % for a 95 % interval with
  12 members, so judge `coverage` against that and not against
  `conf_int` alone;

- `bias` - mean of (ensemble mean - observation);

- `rmse` - root mean square error of the ensemble mean;

- `spread` - root of the mean ensemble variance, i.e. the typical
  standard deviation among members;

- `spread_skill` - `spread / rmse`. About 1 for a reliable ensemble,
  below 1 when the ensemble is under-dispersed (over-confident) and
  above 1 when it is over-dispersed;

- `crps` - mean continuous ranked probability score, which rewards an
  ensemble that is both accurate and sharp. It has the units of the
  variable and lower is better; for a one-member ensemble it reduces to
  the absolute error.

Each observation is one case, so two observations at the same date and
depth count twice. Cases where fewer than two members have a value are
dropped.

## Usage

``` r
score_ensemble(x, conf_int = 0.95, ...)
```

## Arguments

- x:

  an
  [`ensemble_summary()`](https://limnotrack.com/AEME/reference/ensemble_summary.md)
  result, or an `Aeme` object carrying ensemble output. The summary must
  have been built with `keep_members = TRUE` and observations
  (`add_obs = TRUE`), and for a depth-resolved variable with `depths`,
  because members are only comparable with an observation at a common
  depth.

- conf_int:

  numeric; central interval used for `coverage`. Default 0.95.

- ...:

  passed to
  [`ensemble_summary()`](https://limnotrack.com/AEME/reference/ensemble_summary.md)
  when `x` is an `Aeme` object, for example `model`, `vars_sim` and
  `depths`. Ignored for a summary.

## Value

A data.frame with one row per model, variable and depth
(`scope = "depth"`), plus one pooled row per model and variable over all
depths (`scope = "overall"`, `depth = NA`). A variable without a depth
dimension has only the pooled row. Columns are `Model`, `var_sim`,
`depth`, `scope`, `n_obs`, `coverage`, `bias`, `rmse`, `spread`,
`spread_skill` and `crps`. The central interval used is in
`attr(, "conf_int")`.

## See also

[`ensemble_summary()`](https://limnotrack.com/AEME/reference/ensemble_summary.md),
[`plot_ensemble()`](https://limnotrack.com/AEME/reference/plot_ensemble.md),
[`run_aeme_ensemble()`](https://limnotrack.com/AEME/reference/run_aeme_ensemble.md)

## Examples

``` r
if (FALSE) { # \dontrun{
s <- ensemble_summary(aeme, model = "glm_aed", vars_sim = "HYD_temp",
                      depths = c(1, 5, 10))
score_ensemble(s)

# Or straight from an Aeme object with ensemble output
score_ensemble(aeme, model = "glm_aed", vars_sim = "HYD_temp",
               depths = c(1, 5, 10))
} # }
```
