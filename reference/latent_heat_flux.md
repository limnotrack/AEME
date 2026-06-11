# Latent heat flux from a lake surface

Calculates latent heat flux from a lake surface using the bulk
aerodynamic method. Flux is capped at zero — only evaporative loss from
the water is retained (condensation is excluded).

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

- prsttn:

  Numeric. Atmospheric pressure (hPa). Default 981.9.

- Ce:

  Numeric. Bulk transfer coefficient (Dalton number). Default 0.0013.

- rho_air:

  Numeric. Air density (kg/m³). Default 1.168.

- Lv:

  Numeric. Latent heat of vaporisation (J/kg). Default 2453000.

## Value

Numeric. Latent heat flux (W/m²), \\\leq 0\\.

## Details

The flux is calculated as: \$\$Q\_{lh} =
\min\\\left(\frac{0.622}{P}\\C_e\\\rho\_{air}\\L_v\\U\\(e_a - e_s),\\
0\right)\$\$

where \\e_s\\ is the saturation vapour pressure at the water surface
computed by
[`sat_vapour_pressure`](https://limnotrack.com/reference/sat_vapour_pressure.md).

## See also

[`sat_vapour_pressure`](https://limnotrack.com/reference/sat_vapour_pressure.md),
[`flux_to_evap`](https://limnotrack.com/reference/flux_to_evap.md)

## Examples

``` r
latent_heat_flux(Ts = 20, wndspd = 3, prvapr = 10)
#> [1] -94.68515
```
