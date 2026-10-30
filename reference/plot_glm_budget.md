# Box-and-arrow figure of the C, N and P budget of a lake

Draws the budget from
[`diag_glm_budget()`](https://limnotrack.com/AEME/reference/diag_glm_budget.md)
as a lake with its compartments (inorganic, plankton, dissolved and
particulate organic matter), the exchange with the atmosphere above and
the sediment below, and the inflow and outflow, one panel per element.
The number on each arrow is the mean flux between the two boxes and the
width of the arrow scales with it; the number in each box is its mean
inventory. Anything between the compartments that the diagnostics do not
account for is shown as a residual, in red: on the lake as a whole (how
much of the change in the inventory is unexplained) and, with
`show_residual = TRUE`, on each compartment.

## Usage

``` r
plot_glm_budget(
  budget,
  element = NULL,
  period = NULL,
  units = c("kg_yr", "mmol_m2_d"),
  show_residual = FALSE,
  ...
)
```

## Arguments

- budget:

  a `glm_budget` from
  [`diag_glm_budget()`](https://limnotrack.com/AEME/reference/diag_glm_budget.md),
  or an Aeme object, in which case the budget is calculated with the
  other arguments.

- element:

  character; any of `"C"`, `"N"` and `"P"`. Default is the elements in
  `budget`.

- period:

  character; the period of `budget` to draw. Default is the first.

- units:

  character; `"kg_yr"` (default, kg of the element per year) or
  `"mmol_m2_d"` (mmol per m2 of lake surface per day).

- show_residual:

  logical; also show the residual of each compartment?

- ...:

  passed to
  [`diag_glm_budget()`](https://limnotrack.com/AEME/reference/diag_glm_budget.md)
  when `budget` is an Aeme object.

## Value

a ggplot object.

## Details

Fluxes between the same two boxes are added, so a pair of arrows in
opposite directions is only drawn once, the net of the two, in the
direction of the larger. Internal conversions within a box (such as
nitrification) are printed inside it.

## See also

[`diag_glm_budget()`](https://limnotrack.com/AEME/reference/diag_glm_budget.md),
[`calc_glm_fluxes()`](https://limnotrack.com/AEME/reference/calc_glm_fluxes.md)

## Examples

``` r
if (FALSE) { # \dontrun{
plot_glm_budget(aeme)
bud <- diag_glm_budget(aeme, element = "N", period = "year")
plot_glm_budget(bud, period = "2021", units = "mmol_m2_d")
} # }
```
