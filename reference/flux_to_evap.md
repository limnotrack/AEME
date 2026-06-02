# Convert latent heat flux to evaporation depth

Converts latent heat flux (W/m²) to an evaporation rate in metres per
day, suitable for lake water balance calculations.

## Usage

``` r
flux_to_evap(Qlh, Lv = 2453000, rho_water = 1000)
```

## Arguments

- Qlh:

  Numeric. Latent heat flux (W/m²), should be \<= 0.

- Lv:

  Numeric. Latent heat of vaporisation (J/kg). Default 2453000.

- rho_water:

  Numeric. Water density (kg/m³). Default 1000.

## Value

Numeric. Evaporation rate (m/day), \<= 0.

## See also

[`latent_heat_flux()`](https://limnotrack.com/reference/latent_heat_flux.md)

## Examples

``` r
flux_to_evap(-50)
#> Error in flux_to_evap(-50): could not find function "flux_to_evap"

# Full pipeline
Qlh  <- latent_heat_flux(Ts = data$sst, wndspd = data$MET_wndspd, prvapr = data$MET_prvapr)
#> Error in latent_heat_flux(Ts = data$sst, wndspd = data$MET_wndspd, prvapr = data$MET_prvapr): could not find function "latent_heat_flux"
evap <- flux_to_evap(Qlh)
#> Error in flux_to_evap(Qlh): could not find function "flux_to_evap"
```
