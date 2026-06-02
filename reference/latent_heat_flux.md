# Latent heat flux

Calculates latent heat flux from a lake surface using the bulk
aerodynamic method. Flux is capped at zero — only heat loss from the
water is retained.

## Usage

``` r
latent_heat_flux(
  Ts,
  wndspd,
  prvapr,
  prsttn = 981.9,
  Ce = 0.0013,
  rho_air = 1.168,
  Lv = 2453000
)
```

## Arguments

- Ts:

  Numeric. Water surface temperature (°C).

- wndspd:

  Numeric. Wind speed (m/s).

- prvapr:

  Numeric. Air vapour pressure (hPa).

- Ce:

  Numeric. Bulk transfer coefficient (Dalton number). Default 0.0013.

- rho_air:

  Numeric. Air density (kg/m³). Default 1.168.

- Lv:

  Numeric. Latent heat of vaporisation (J/kg). Default 2453000.

- P:

  Numeric. Atmospheric pressure (hPa). Default 981.9.

## Value

Numeric. Latent heat flux (W/m²), \<= 0.

## See also

[`sat_vapour_pressure()`](https://limnotrack.com/reference/sat_vapour_pressure.md),
[`flux_to_evap()`](https://limnotrack.com/reference/flux_to_evap.md)

## Examples

``` r
latent_heat_flux(Ts = 20, wndspd = 3, prvapr = 10)
#> Error in latent_heat_flux(Ts = 20, wndspd = 3, prvapr = 10): could not find function "latent_heat_flux"

# Vectorised over a data frame
latent_heat_flux(Ts     = data$sst,
                 wndspd = data$MET_wndspd,
                 prvapr = data$MET_prvapr)
#> Error in latent_heat_flux(Ts = data$sst, wndspd = data$MET_wndspd, prvapr = data$MET_prvapr): could not find function "latent_heat_flux"
```
