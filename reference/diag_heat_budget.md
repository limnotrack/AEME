# Whole-lake heat budget vs observations: net deficit/surplus, or just surface/bottom redistribution?

Uses `HYD_temp` (not `HYD_nrgcnt` directly - there is no independent
observation of energy density to compare against, only temperature) for
the bias itself, volume-weighted via the lake's own hypsography, then
reports the implied total heat-content bias in Joules for an intuitive
magnitude (`rho * cw * volume-weighted-bias * lake_volume`).

## Usage

``` r
diag_heat_budget(
  aeme,
  model,
  depth_breaks = c(-0.01, 2, 6, 10, 15, 20, 100),
  bias_ok_degC = 0.5,
  redistribution_ratio = 0.5
)
```

## Arguments

- aeme:

  Aeme object; already built and run for `model`.

- model:

  character; single model code to diagnose (see
  [`check_model`](https://limnotrack.com/reference/check_model.md)).

- depth_breaks:

  numeric vector; passed to [`cut`](https://rdrr.io/r/base/cut.html) for
  the by-depth-band summary.

- bias_ok_degC:

  numeric; below this absolute volume-weighted bias, classified `"ok"`.

- redistribution_ratio:

  numeric; if the volume-weighted bias's magnitude is below this
  fraction of the largest single-band absolute bias, the bands are
  judged to be substantially cancelling each other out (heat misplaced,
  not missing/excess) rather than agreeing (a genuine net deficit or
  surplus).

## Value

list with `volume_weighted_bias_degC`, `total_bias_J`, `lake_volume_m3`,
`by_depth_band` (data frame), `classification` (one of `"deficit"`,
`"surplus"`, `"redistribution"`, `"ok"`, `"no_data"`), `detail`.
