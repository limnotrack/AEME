# Estimate Surface Temperature Using Energy Balance Model

Estimate Surface Temperature Using Energy Balance Model

## Usage

``` r
estimate_surface_temperature(
  df,
  depth,
  init_temp = NULL,
  f_mix = 0.2,
  rho_w = 1000,
  cp_w = 4186,
  dt = 86400,
  alpha_sw = 0.07,
  ch = 0.0013,
  ce = 0.0013,
  relax_tau = 3 * 86400
)
```

## Arguments

- df:

  data frame with meteorological and observed surface temperature data.
  Must include columns:

  - `Date`: Date of observation

  - `MET_tmpair`: Air temperature (°C)

  - `MET_wndspd`: Wind speed (m/s)

  - `MET_radswd`: Downward shortwave radiation (W/m²)

  - `MET_radlwd`: Downward longwave radiation (W/m²)

  - `MET_prvapr`: Vapor pressure (mb)

  - `HYD_temp`: Observed surface temperature (°C),

  - `T5avg`: 5-day average air temperature (°C)

- depth:

  Lake depth (m)

- init_temp:

  Initial surface temperature (°C). If NULL, uses first available
  observation or air temperature.

- f_mix:

  Fraction of lake involved in surface mixing. Defaults to 0.2

- rho_w:

  Water density (kg/m³)

- cp_w:

  Heat capacity of water (J/kg/K)

- dt:

  Timestep (s)

- alpha_sw:

  Shortwave albedo. Defaults to 0.07

- ch:

  Bulk sensible heat coefficient. Defaults to 1.3e-3

- ce:

  Bulk latent heat coefficient. Defaults to 1.3e-3

- relax_tau:

  Relaxation timescale to observations (s)

## Value

Data frame with estimated surface temperature added as column `sst`
