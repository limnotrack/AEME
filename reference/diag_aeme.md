# Diagnose the three baseline budgets (water, heat, nutrient) at once

Runs
[`diag_water_balance`](https://limnotrack.com/reference/diag_water_balance.md),
[`diag_heat_budget`](https://limnotrack.com/reference/diag_heat_budget.md)
and, when `use_bgc = TRUE`,
[`diag_nutrient_budget`](https://limnotrack.com/reference/diag_nutrient_budget.md)
against an already built and run
[`Aeme-class`](https://limnotrack.com/reference/Aeme.md) object, and
reports a one-line classification for each. Meant as the entry point for
a baseline (shipped-default) simulation, before committing to a staged
calibration design: which parameters go in which stage, and what bounds,
depends on which of these budgets is actually biased.

## Usage

``` r
diag_aeme(aeme, model, use_bgc, ...)
```

## Arguments

- aeme:

  Aeme object; already built and run for `model`.

- model:

  character; single model code to diagnose (see
  [`check_model`](https://limnotrack.com/reference/check_model.md)).

- use_bgc:

  logical; also run
  [`diag_nutrient_budget`](https://limnotrack.com/reference/diag_nutrient_budget.md).
  Set to `FALSE` when `aeme` was run without the BGC module active.
  Automatically derived from the `use_bgc` flag in the `aeme` object
  when omitted.

## Value

an `aeme_diag` object: list with `aeme`, `model`, `water_balance`,
`heat_budget` and `nutrient_budget` (`NULL` when `use_bgc = FALSE`).

## Details

`aeme` must already have current output for `model` (i.e. built with
[`build_aeme`](https://limnotrack.com/reference/build_aeme.md) and run
with [`run_aeme`](https://limnotrack.com/reference/run_aeme.md)) - this
function does not build or run anything itself.
