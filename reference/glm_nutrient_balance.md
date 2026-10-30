# Whole-lake N and P budget from the GLM-AED mass balance

Closes the nitrogen and phosphorus budget of a GLM-AED run from its
`mass_balance.csv`, in kg, and summarises it by year, month or the whole
run. The components of total N (TN) and total P (TP) are summed as in
[`calc_aed_totals_balance()`](https://limnotrack.com/AEME/reference/calc_aed_totals_balance.md)
(so they follow the AED configuration, including the phytoplankton
internal nutrient quotas) and the budget is closed as in
[`diag_glm_mass_balance()`](https://limnotrack.com/AEME/reference/diag_glm_mass_balance.md):

## Usage

``` r
glm_nutrient_balance(
  aeme,
  path = NULL,
  element = c("N", "P"),
  period = c("year", "month", "all"),
  include_spin_up = TRUE
)
```

## Arguments

- aeme:

  Aeme object that has been run with `glm_aed`.

- path:

  character; root directory used with `aeme`.

- element:

  character; `"N"`, `"P"` or both.

- period:

  character; summary resolution, `"year"` (default), `"month"` or
  `"all"` (the whole run).

- include_spin_up:

  logical; keep the spin-up period? If `FALSE`, rows before the start of
  the simulation are dropped.

## Value

a list with `daily` (the per-row budget from
[`diag_glm_mass_balance()`](https://limnotrack.com/AEME/reference/diag_glm_mass_balance.md)
in kg, with an `element` column) and `summary` (per `element` and
`period`: `inflow`, `outflow`, `retained` = inflow - outflow,
`d_storage`, `internal`, all in kg, and `retention_pct` = 100 \*
retained / inflow). The first row has no previous storage and is
excluded from the summary.

## Details

`internal = d(storage) - (inflow - outflow)`

Reactions between pools cancel in the totals, so `internal` is the net
amount gained (sediment release, atmospheric deposition, N fixation) or
lost (burial, denitrification, settling to the sediment, seepage) across
the lake boundary that the inflow and outflow terms do not see. A
persistently large `internal` points to a missing source or sink. `Inf`
and `Out` are totals over the save interval, so the sums are valid
whatever `nsave` is.

## See also

[`set_glm_balance_groups()`](https://limnotrack.com/AEME/reference/set_glm_balance_groups.md)
to choose which variables GLM writes to the mass balance.

## Examples

``` r
if (FALSE) { # \dontrun{
nb <- glm_nutrient_balance(aeme, element = "N", period = "year")
nb$summary
} # }
```
