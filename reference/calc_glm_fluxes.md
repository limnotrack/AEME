# Whole-lake C, N and P fluxes from the GLM-AED diagnostic output

Integrates the diagnostic variables that libaed-water writes to the GLM
`output.nc` (process rates per m3, and fluxes across the sediment-water
and air-water interfaces per m2) over the lake, giving mmol per day for
each process. This is the basis of the budget returned by
[`diag_glm_budget()`](https://limnotrack.com/AEME/reference/diag_glm_budget.md)
and drawn by
[`plot_glm_budget()`](https://limnotrack.com/AEME/reference/plot_glm_budget.md):
it shows where each element comes from and goes to, and so where a
concentration that is out of balance is being made or lost.

## Usage

``` r
calc_glm_fluxes(
  aeme = NULL,
  file = NULL,
  element = c("N", "P", "C"),
  path = NULL,
  settling = c("hypsograph", "interface")
)
```

## Arguments

- aeme:

  Aeme object that has been run with `glm_aed`. Not needed if `file` is
  given.

- file:

  character; path to the GLM `output.nc`. If `NULL` it is found from
  `aeme`.

- element:

  character; any of `"C"`, `"N"` and `"P"`.

- path:

  character; root directory used with `aeme`.

- settling:

  character; how settling to the sediment is calculated, `"hypsograph"`
  (default) or `"interface"`. See Details.

## Value

a tibble with columns `Date`, `element`, `process`, `from`, `to`, `var`
(the netCDF variable) and `value` (mmol/d, daily mean). The attribute
`missing` lists the flux patterns for which GLM wrote no variable.

## Details

Volumetric rates (`mmol/m3/d`) are multiplied by the layer volumes,
sediment zone fluxes (`*_Z`, `mmol/m2/d`) by `zarea` and surface fluxes
by `surface_area`. Each flux is positive in the direction `from` to `to`
(see the `from` and `to` columns), so the sediment-water exchange
`sediment` to `pom` is negative when particles are settling out.

Only variables that GLM wrote are used. AED writes most fluxes only when
the module `diag_level` is high enough (`diag_level = 10` gives
everything; the `*_swi` terms need at least 2) and the `_Z` zone
variables need sediment zones, so a run with fewer diagnostics gives a
less complete picture. The variables that were looked for but not found
are listed in the `missing` attribute of the result.

Denitrification is the sum of `NIT_denit` and, when the organic matter
module is coupled to nitrate (`simDenitrification`), `OGM_doc_denit`,
which holds nitrogen despite its carbon label.

Settling to the sediment is worked out from the per-layer settling rates
(`OGM_pon_set`, `PHY_<group>_set_n`, ...) and the hypsograph:
`sum(rate * layer thickness * (A[k] - A[k - 1]))`, where the area change
across a layer is the sediment that particles settling through it can
land on. The sediment-water interface diagnostics (`OGM_pon_swi_Z`,
`PHY_phy_swi_*`) report the same thing but in the lake tested here were
10 to 30 times larger than `vvel * concentration` allows (AED scales the
layer rate by a thickness that is not that of the layer), and the water
column inventory only closes with the hypsograph-based value. Use
`settling = "interface"` to see the interface values instead.

## Examples

``` r
if (FALSE) { # \dontrun{
fx <- calc_glm_fluxes(aeme, element = "N")
dplyr::summarise(fx, mean = mean(value), .by = c(process, from, to))
} # }
```
