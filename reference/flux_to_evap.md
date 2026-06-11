# Convert latent heat flux to evaporation depth

Converts latent heat flux (W/m²) to an evaporation rate in metres per
day, suitable for lake water balance calculations.

## Usage

``` r
flux_to_evap(Qlh, Lv = 2453000, rho_water = 1000)
```

## Arguments

- Qlh:

  Numeric. Latent heat flux (W/m²), should be \\\leq 0\\.

- Lv:

  Numeric. Latent heat of vaporisation (J/kg). Default 2453000.

- rho_water:

  Numeric. Water density (kg/m³). Default 1000.

## Value

Numeric. Evaporation rate (m/day), \\\leq 0\\.

## Details

The conversion is: \$\$E = \frac{Q\_{lh}}{L_v\\\rho_w} \times 86400\$\$

## See also

[`latent_heat_flux`](https://limnotrack.com/reference/latent_heat_flux.md)

## Examples

``` r
flux_to_evap(-50)
#> [1] -0.001761109
```
