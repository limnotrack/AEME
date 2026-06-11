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
#> [1] 23.37685
sat_vapour_pressure(c(15, 20, 25))
#> [1] 17.04927 23.37685 31.66942
```
