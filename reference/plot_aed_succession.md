# Plot phytoplankton succession

Two-panel summary of phytoplankton succession. The top panel is a time
series of total chlorophyll a (`PHY_tchla`); the bottom panel is a
monthly composite showing the relative contribution (0-100 %) of each
phytoplankton group to the total biomass, with one stacked bar per
month. The panels share a date axis and the bottom panel takes two
thirds of the height.

## Usage

``` r
plot_aed_succession(
  aeme,
  model,
  groups = NULL,
  depth = 0,
  remove_spin_up = TRUE,
  ens_n = 1
)
```

## Arguments

- aeme:

  Aeme object.

- model:

  character vector; model(s) to plot. If missing, all models in the Aeme
  object will be plotted.

- groups:

  character vector of phytoplankton group names, matching the
  `PHY_<group>` variables simulated in `aeme` (e.g.
  `c("cyano", "green", "diatom")`). Default is `NULL`, which uses all
  simulated `PHY_` variables other than `PHY_tchla`.

- depth:

  numeric; depth (m, surface-referenced) at which the variables are
  evaluated. Default `0` (surface).

- remove_spin_up:

  logical; remove spin-up period from plot. Default is TRUE.

- ens_n:

  integer; ensemble number to plot. Default is 1.

## Value

A patchwork object of two ggplot panels.

## See also

[`aed_succession_index()`](https://limnotrack.com/AEME/reference/aed_succession_index.md)
