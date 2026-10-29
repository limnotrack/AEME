# Diagnose the GLM-AED mass balance

Computes, for each variable (or each AED total), the change in the
in-lake inventory and the net external transport, and the remainder
between them:

## Usage

``` r
diag_glm_mass_balance(
  mb = NULL,
  by = c("total", "var"),
  aed_totals = NULL,
  path_glm = NULL,
  units = c("mmol", "kg"),
  plot = FALSE,
  aeme = NULL,
  path = NULL
)
```

## Arguments

- mb:

  tibble from
  [`read_glm_mass_balance()`](https://limnotrack.com/AEME/reference/read_glm_mass_balance.md),
  or a path to `mass_balance.csv`. If `NULL`, it is read from `aeme`.

- by:

  `"total"` (default) to diagnose the aed totals, or `"var"` to diagnose
  each variable in the file.

- aed_totals:

  list; the `aed_totals` block (`TN_vars`, `TN_varscale`, ...), as built
  by `derive_aed_totals()`. Ignored if `path_glm` is given.

- path_glm:

  character; GLM-AED model directory (containing `aed/`) to derive
  `aed_totals` from. Takes precedence over `aed_totals`.

- units:

  `"mmol"` (default, AED native) or `"kg"`, which converts using the
  molar masses of N, P and C. `TSS` is left unchanged when converted.

- plot:

  logical; also return a ggplot of cumulative `net_internal`.

- aeme:

  Aeme object. Used to read `mb` when it is `NULL`, and to locate the
  `glm_aed` configuration for the totals when neither `aed_totals` nor
  `path_glm` is given.

- path:

  character; root directory used with `aeme`.

## Value

a list with `daily` (tibble: `datetime`, `name`, `interval_days`,
`inflow`, `outflow`, `lake`, `d_lake`, `net_external`, `net_internal`,
`cum_net_internal`), `summary` (tibble: one row per `name`, with the
mean inventory, total external flux, total internal remainder and its
percentage of the mean inventory) and, if `plot = TRUE`, `plot`.

## Details

`net_internal = d(Lake) - (Inf - Out)`

where `d(Lake)` is the change in the `Lake` term since the previous row
and `Inf`/`Out` are the inflow and outflow totals over that same
interval. GLM writes `Inf` and `Out` as totals over the save interval
(`nsave * dt`), so the interval length of each row is taken from the
time stamps and per-day rates (`*_rate`, mmol per day) are returned
alongside the interval totals; use the rates to compare runs with
different `nsave`. If `path_glm` (or `aeme`) is given, the time stamps
are checked against `nsave * dt` in the GLM nml. `net_internal` is the
amount gained or lost inside the lake (reactions, sediment and
atmospheric exchange, repair of negative states). For conservative
quantities it should be close to zero. For reactive variables it is
expected to be non-zero, but for the totals TN, TP and TOC it is the net
sediment/atmosphere exchange and should be small relative to the
inventory.
