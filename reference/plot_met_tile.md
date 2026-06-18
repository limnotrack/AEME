# Plot a tile plot of meteorological data

Plot a tile plot of meteorological data

## Usage

``` r
plot_met_tile(aeme, var_aeme = "MET_tmpair", use_hydro_year = TRUE, var_inp)
```

## Arguments

- aeme:

  Aeme object.

- var_aeme:

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

  Logical. If `TRUE`, the hydrological year is used. The hydrological
  year starts in October for the northern hemisphere and in July for the
  southern hemisphere. If `FALSE`, the calendar year is used.

- var_inp:

  Character. **\[deprecated\]** Use `var_aeme` instead.

## Value

A ggplot object
