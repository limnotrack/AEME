# Carbon, nitrogen and phosphorus budget of a GLM-AED lake

Combines the process and interface fluxes from the GLM-AED diagnostic
output
([`calc_glm_fluxes()`](https://limnotrack.com/AEME/reference/calc_glm_fluxes.md))
with the inflow, outflow and in-lake inventory from the mass balance
([`diag_glm_mass_balance()`](https://limnotrack.com/AEME/reference/diag_glm_mass_balance.md))
to give, for each element, where it comes from, where it goes, and how
much of the change in the inventory those terms explain.

## Usage

``` r
diag_glm_budget(
  aeme,
  element = c("N", "P", "C"),
  period = c("all", "year", "month"),
  path = NULL,
  include_spin_up = TRUE,
  settling = c("hypsograph", "interface")
)
```

## Arguments

- aeme:

  Aeme object that has been run with `glm_aed`. Not needed if `file` is
  given.

- element:

  character; any of `"C"`, `"N"` and `"P"`.

- period:

  character; summary resolution, `"all"` (default, the whole run),
  `"year"` or `"month"`.

- path:

  character; root directory used with `aeme`.

- include_spin_up:

  logical; keep the spin-up period? If `FALSE`, days before the
  simulation start are dropped.

- settling:

  character; `"hypsograph"` (default) or `"interface"`, see
  [`calc_glm_fluxes()`](https://limnotrack.com/AEME/reference/calc_glm_fluxes.md).

## Value

a list with

- `fluxes`: one row per element, period and process between two
  compartments (`from`, `to`), with the mean rate `mmol_d`, the
  equivalent `kg_yr` (kg of the element per year) and the areal
  `mmol_m2_d` (per m2 of mean lake surface).

- `pools`: the mean inventory of each compartment (`mmol` and `kg`),
  from the AED totals of the model configuration.

- `closure`: per element and period, the `term`s `inflow`, `outflow`,
  `sediment`, `atmosphere`, `explained`, `d_storage` and `residual`, in
  the same three units. Positive means a gain for the lake.

- `compartments`: the same closure for each compartment (`inorganic`,
  `phyto`, `dom`, `pom`): its `d_storage` and the `inflow`, `outflow`,
  `flux_in` and `flux_out` of every diagnostic flux entering or leaving
  it, including from other compartments, and the `residual` they do not
  explain. A compartment with a large residual has a process or exchange
  that is mis-reported or missing: this is where to look when a
  concentration drifts.

- `daily`: daily net boundary flux and mass balance terms behind
  `closure`, in mmol/d.

## Details

The lake is treated as one pool made of the compartments `inorganic`,
`phyto`, `dom` and `pom` (for carbon, `inorganic` is DIC and is outside
the pool, as it is not part of TOC). A flux that crosses the boundary of
that pool adds to or removes from the inventory. Anything between two
compartments inside the pool (uptake, mortality, mineralisation,
nitrification, ...) is internal and cancels. The closure is

`residual = d(inventory) - (inflow - outflow + sediment + atmosphere)`

where `sediment` is the net exchange across the sediment-water interface
(dissolved release minus settling) and `atmosphere` is deposition and
gas exchange, including N2 fixation and denitrification for nitrogen. A
residual near zero means the diagnostics account for the budget. A
persistent residual points to a source or sink the diagnostics do not
report, for example a module that is not writing its `*_swi` terms, or a
mismatch between how a pool is settled and how it is resuspended.

The fluxes are the instantaneous rates AED writes at each save time. On
the example lake a residual of 3 to 13% of the gross primary production
remained with settling, hydrolysis, resuspension and outflow switched
off, with hourly instead of daily output, with a ten times shorter time
step and with thinner layers, and it disappeared when growth was
switched off. It therefore follows the production terms (`PHY_upt_*`)
rather than the other pathways, and is not a sampling or resolution
effect. Compare the residual with the largest flux, not with the change
in storage, which can be close to zero.

The inflow, outflow and inventory need the totals in the mass balance:
`TN`, `TP` and `TOC` are all in the default `balance_vars` (see
[`set_glm_balance_groups()`](https://limnotrack.com/AEME/reference/set_glm_balance_groups.md)
and
[`add_glm_balance_vars()`](https://limnotrack.com/AEME/reference/add_glm_balance_vars.md)).
Without them `closure` is `NULL` for that element but the fluxes and
pools are still returned.

## See also

[`plot_glm_budget()`](https://limnotrack.com/AEME/reference/plot_glm_budget.md)
for a box-and-arrow figure of the result.

## Examples

``` r
if (FALSE) { # \dontrun{
bud <- diag_glm_budget(aeme, element = c("N", "P"))
bud$closure
plot_glm_budget(bud)
} # }
```
