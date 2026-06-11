# Plot Water Balance Components as Timeseries

Plot Water Balance Components as Timeseries

## Usage

``` r
plot_wbal_ts(
  aeme,
  var_aeme = c("LKE_lvlwtr", "LKE_inflow", "LKE_outflow", "LKE_pcpvol", "LKE_Qe",
    "HYD_surft"),
  add_model = TRUE
)
```

## Arguments

- aeme:

  Aeme object with model output and observations added. Must have model
  output [`run_aeme()`](https://limnotrack.com/reference/run_aeme.md)

- add_model:

  logical; whether to overlay model output on the estimated components.
  Defaults to TRUE.

- vars:

  character vector of AEME variable names to plot. Options are
  `"LKE_lvlwtr"`, `"LKE_inflow"`, `"LKE_outflow"`, `"LKE_Qe"`,
  `"HYD_surft"`. Defaults to all.

## Value

ggplot object
