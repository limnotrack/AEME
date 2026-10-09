# Sanity-check lake loading against simple hydrological benchmarks

Flags discharge and load values that look implausible relative to the
lake's own volume and surface area, or relative to broad plausible
concentration ranges (the same ranges applied at ingestion by
[`standardise_inflow()`](https://limnotrack.com/reference/standardise_inflow.md)).
This is a lightweight order-of-magnitude sanity check – not a substitute
for expert review – intended to catch things like unit errors (e.g. m3/s
mistaken for m3/day), a lake volume/area that doesn't match the inflow
data, or forcing data that has been mis-scaled.

## Usage

``` r
check_loads(aeme, loads = NULL, verbose = TRUE)
```

## Arguments

- aeme:

  Aeme object.

- loads:

  data.frame; output of `summarise_loads(aeme, by_inflow = FALSE)`. If
  `NULL` (default), it is calculated internally.

- verbose:

  logical; if `TRUE` (default), emit a `cli_warn` for every flagged
  check.

## Value

A data frame with columns `check`, `value`, `unit`, `lower`, `upper`,
`flag` (`TRUE` if `value` falls outside `[lower, upper]`) and `message`
(`NA` when not flagged).

## Checks performed

- **residence_time_days**: lake volume divided by annual discharge, in
  days. Flagged if outside `[1, 36525]` (1 day to 100 years) – a lake
  flushing in under a day, or essentially never, usually indicates a
  units or magnitude problem rather than a real lake.

- **hydraulic_loading_m_yr**: annual discharge divided by lake surface
  area, in metres of water per year. Flagged if outside `[0.01, 1e5]`
  m/yr.

- **implied_conc\_\***: the flow-weighted mean concentration implied by
  `load / discharge` for each of `NIT_amm`, `NIT_nit`, `PHS_frp`,
  `CAR_doc` and `CHM_oxy` (when present), checked against the same
  plausible ranges used by
  [`standardise_inflow()`](https://limnotrack.com/reference/standardise_inflow.md).

Thresholds are deliberately loose (order-of-magnitude) so that only
clearly implausible values are flagged.

## Examples

``` r
aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
aeme <- readRDS(aeme_file)
check_loads(aeme)
#>                    check        value unit lower  upper  flag message
#> 1    residence_time_days 1.029366e+02 days  1.00  36525 FALSE    <NA>
#> 2 hydraulic_loading_m_yr 2.570428e+01 m/yr  0.01 100000 FALSE    <NA>
#> 3   implied_conc_NIT_amm 2.961474e-03 g/m3  0.00     10 FALSE    <NA>
#> 4   implied_conc_NIT_nit 8.953549e-04 g/m3  0.00     10 FALSE    <NA>
#> 5   implied_conc_PHS_frp 1.043143e-03 g/m3  0.00      5 FALSE    <NA>
#> 6   implied_conc_CAR_doc 9.780457e-03 g/m3  0.00    100 FALSE    <NA>
```
