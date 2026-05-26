# Return station pressure from mean sea level pressure.

Return station pressure from mean sea level pressure.

## Usage

``` r
get_station_pressure(prmslp, elevation, tmpair)
```

## Arguments

- prmslp:

  A numeric vector of mean sea level pressure in Pa

- elevation:

  A numeric vector of elevation in m

- tmpair:

  A numeric vector of air temperature in degC

## Value

A numeric vector of station pressure in Pa

## Note

This function is just the inverse of
[`get_mean_sea_level_pressure`](https://limnotrack.com/reference/get_mean_sea_level_pressure.md).

## References

See
[`get_mean_sea_level_pressure`](https://limnotrack.com/reference/get_mean_sea_level_pressure.md).

## Examples

``` r
get_station_pressure(101226.5, 105:205, 17.19)
#>   [1] 99984.96 99973.22 99961.49 99949.76 99938.02 99926.29 99914.56 99902.84
#>   [9] 99891.11 99879.39 99867.67 99855.95 99844.23 99832.51 99820.80 99809.08
#>  [17] 99797.37 99785.66 99773.95 99762.24 99750.54 99738.83 99727.13 99715.43
#>  [25] 99703.73 99692.03 99680.34 99668.64 99656.95 99645.26 99633.57 99621.88
#>  [33] 99610.20 99598.51 99586.83 99575.15 99563.47 99551.79 99540.11 99528.44
#>  [41] 99516.77 99505.10 99493.43 99481.76 99470.09 99458.43 99446.76 99435.10
#>  [49] 99423.44 99411.78 99400.13 99388.47 99376.82 99365.17 99353.52 99341.87
#>  [57] 99330.22 99318.58 99306.93 99295.29 99283.65 99272.01 99260.37 99248.74
#>  [65] 99237.10 99225.47 99213.84 99202.21 99190.58 99178.96 99167.33 99155.71
#>  [73] 99144.09 99132.47 99120.85 99109.24 99097.62 99086.01 99074.40 99062.79
#>  [81] 99051.18 99039.57 99027.97 99016.37 99004.77 98993.17 98981.57 98969.97
#>  [89] 98958.38 98946.78 98935.19 98923.60 98912.01 98900.42 98888.84 98877.26
#>  [97] 98865.67 98854.09 98842.51 98830.94 98819.36
```
