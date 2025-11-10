# Plot a tile plot of meteorological data

Plot a tile plot of meteorological data

## Usage

``` r
plot_met_tile(aeme, var_inp = "MET_tmpair", use_hydro_year = TRUE)
```

## Arguments

- aeme:

  aeme; object.

- var_inp:

  Character. Variable to plot. Can be one of:

  - `"MET_tmpair"`: Air temperature

  - `"MET_pprain"`: Rainfall

  - `"MET_wndspd"`: Wind speed

  - `"MET_humrel"`: Relative humidity

  - `"MET_radswd"`: Shortwave radiation

  - `"MET_radlwd"`: Longwave radiation

  - `"MET_pres"`: Atmospheric pressure

  - `"MET_ppsnow"`: Snowfall

  - `"MET_wnddir"`: Wind direction

- use_hydro_year:

  Logical. If `TRUE`, the hydrological year is used.

## Value

A ggplot object
