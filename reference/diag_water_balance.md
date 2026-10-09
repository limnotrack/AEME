# Water level bias and drift, against observations and against the model's own starting point

Reports two independent drift signals:

- `drift_m_per_yr` - obs-based: slope of the (sim - obs) residual over
  time, at whatever observed dates exist. Needs real observations, and
  enough of them to trust a slope.

- `sim_drift_m_per_yr` - model-only: slope of the raw simulated
  `LKE_lvlwtr` series itself
  ([`get_var`](https://limnotrack.com/reference/get_var.md) with
  `use_obs = FALSE`, i.e. referenced to the hypsography's own zero, not
  any observation-derived elevation offset) against time, over every
  simulated day in the post-spin-up window. Available for every lake
  regardless of observation coverage, so a lake that is silently
  draining or filling under its own simulated water balance shows up
  here even with zero level observations to compare against.

## Usage

``` r
diag_water_balance(
  aeme,
  model,
  drift_ok_m_per_yr = 0.05,
  bias_ok_m = NULL,
  min_n = 20
)
```

## Arguments

- aeme:

  Aeme object; already built and run for `model` (see
  [`build_aeme`](https://limnotrack.com/reference/build_aeme.md)/[`run_aeme`](https://limnotrack.com/reference/run_aeme.md)).

- model:

  character; single model code to diagnose (see
  [`check_model`](https://limnotrack.com/reference/check_model.md)).

- drift_ok_m_per_yr:

  numeric; below this absolute slope, classified `"static_offset"` (or
  `"ok"`) rather than `"drift"`.

- bias_ok_m:

  numeric or `NULL`; below this absolute mean bias (in metres),
  classified `"ok"` regardless of drift. When `NULL` (default), derived
  from the lake's maximum depth as `5%` of max depth, clamped to
  \\\[0.1, 0.5\]\\ m, so the threshold scales sensibly across shallow
  and deep lakes without changing the absolute-unit semantics.

- min_n:

  integer; below this many observations, the obs-based signal is not
  trusted (even if computed) and the model-only signal decides the
  classification instead.

## Value

list with `mean_bias`, `drift_m_per_yr`, `n`, `span_yr`,
`sim_drift_m_per_yr`, `sim_change_m`, `sim_span_yr`, `classification`
(one of `"ok"`, `"static_offset"`, `"drift"`, `"ok_no_obs"`,
`"sim_drift"`, `"no_data"`), `low_confidence`, `detail`.
