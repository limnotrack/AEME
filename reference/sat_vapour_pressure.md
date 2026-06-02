# Saturation vapour pressure

Calculates saturation vapour pressure at the water surface using the
Magnus formula.

## Usage

``` r
sat_vapour_pressure(Ts)
```

## Arguments

- Ts:

  Numeric. Water surface temperature (°C).

## Value

Numeric. Saturation vapour pressure (hPa).

## Examples

``` r
sat_vapour_pressure(20)
#> Error in sat_vapour_pressure(20): could not find function "sat_vapour_pressure"
sat_vapour_pressure(c(15, 20, 25))
#> Error in sat_vapour_pressure(c(15, 20, 25)): could not find function "sat_vapour_pressure"
```
