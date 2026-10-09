# Per-variable BGC bias vs observations

Reports bias for whichever candidate variables this lake actually has
usable data for.

## Usage

``` r
diag_nutrient_budget(
  aeme,
  model,
  candidates = c("CHM_oxy", "NIT_amm", "NIT_nit", "NIT_tn", "PHS_frp", "PHS_tp",
    "PHY_tchla"),
  min_obs = 8,
  bias_ok_frac = 0.25
)
```

## Arguments

- aeme:

  Aeme object; already built and run for `model`.

- model:

  character; single model code to diagnose (see
  [`check_model`](https://limnotrack.com/reference/check_model.md)).

- candidates:

  character vector; AEME variable names to check.

- min_obs:

  integer; below this many in-window observations, the variable is
  skipped.

- bias_ok_frac:

  numeric; a variable is classified `"ok"` when the absolute mean bias
  is below this fraction of the observed mean (a variable-appropriate
  relative threshold, since oxygen/nitrate/ chlorophyll live on very
  different absolute scales).

## Value

data frame with columns `var`, `mean_bias`, `mean_obs`, `rel_bias`, `n`,
`direction` (`"model too high"`, `"model too low"` or `"ok"`). Zero rows
if no candidate variable has enough usable data.
