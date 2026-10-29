# Sum mass balance variables into AED totals

Combines the components of the `aed_totals` block (TN, TP, TOC and, when
present, TSS) from a mass balance table, applying the `*_varscale`
factors (as in
[`set_aed_totals()`](https://limnotrack.com/AEME/reference/set_aed_totals.md)).
Because the scaled components are in mmol, the totals are in mmol of N,
P and C respectively.

## Usage

``` r
calc_aed_totals_balance(
  mb,
  aed_totals = NULL,
  path_glm = NULL,
  units = c("mmol", "kg")
)
```

## Arguments

- mb:

  tibble from
  [`read_glm_mass_balance()`](https://limnotrack.com/AEME/reference/read_glm_mass_balance.md).

- aed_totals:

  list; the `aed_totals` block (`TN_vars`, `TN_varscale`, ...), as built
  by `derive_aed_totals()`. Ignored if `path_glm` is given.

- path_glm:

  character; GLM-AED model directory (containing `aed/`) to derive
  `aed_totals` from. Takes precedence over `aed_totals`.

- units:

  `"mmol"` (default, AED native) or `"kg"`, which converts using the
  molar masses of N, P and C. `TSS` is left unchanged when converted.

## Value

a tibble with columns `datetime`, `total` (`"TN"`, `"TP"`, `"TOC"`,
...), `term` (`"Inf"`, `"Lake"`, `"Out"`), `value`, `units` and
`n_missing`, the number of components absent from `mb` (they are dropped
from the sum). Totals with no components in `mb` are skipped.
