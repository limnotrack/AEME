# Return mean sea level pressure given air temperature, elevation and station pressure.

Return mean sea level pressure given air temperature, elevation and
station pressure.

## Usage

``` r
get_mean_sea_level_pressure(prsttn, elevation, tmpair)
```

## Arguments

- elevation:

  A numeric vector of elevation in m

- MET_prsttn:

  A numeric vector of observed station pressure in Pa

- MET_tmpair:

  A numeric vector of air temperature in degC

## Value

A numeric vector of mean sea level pressure in Pa

## Note

The standard procedure for the US is to use for MET_tmpair the average
of the current station temperature and the station temperature from 12
hours ago.

## References

Hess SL, Introduction to theoretical meteorology, Holt Rinehart and
Winston, NY 1959, ch. 6.5; Stull RB, Meteorology for scientists and
engineers, 2nd edition, Brooks/Cole 2000, ch. 1.

## Examples

``` r
get_mean_sea_level_pressure(101226.5, 105:205, 17.19)
#>   [1] 102483.5 102495.5 102507.5 102519.6 102531.6 102543.6 102555.7 102567.7
#>   [9] 102579.7 102591.8 102603.8 102615.9 102627.9 102640.0 102652.0 102664.0
#>  [17] 102676.1 102688.1 102700.2 102712.2 102724.3 102736.4 102748.4 102760.5
#>  [25] 102772.5 102784.6 102796.6 102808.7 102820.8 102832.8 102844.9 102857.0
#>  [33] 102869.0 102881.1 102893.2 102905.2 102917.3 102929.4 102941.5 102953.5
#>  [41] 102965.6 102977.7 102989.8 103001.8 103013.9 103026.0 103038.1 103050.2
#>  [49] 103062.3 103074.3 103086.4 103098.5 103110.6 103122.7 103134.8 103146.9
#>  [57] 103159.0 103171.1 103183.2 103195.3 103207.4 103219.5 103231.6 103243.7
#>  [65] 103255.8 103267.9 103280.0 103292.1 103304.2 103316.3 103328.4 103340.5
#>  [73] 103352.6 103364.8 103376.9 103389.0 103401.1 103413.2 103425.3 103437.5
#>  [81] 103449.6 103461.7 103473.8 103486.0 103498.1 103510.2 103522.3 103534.5
#>  [89] 103546.6 103558.7 103570.9 103583.0 103595.1 103607.3 103619.4 103631.6
#>  [97] 103643.7 103655.8 103668.0 103680.1 103692.3
```
