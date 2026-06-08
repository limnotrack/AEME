# Get the column names for the meteorological variables

Get the column names for the meteorological variables

## Usage

``` r
get_met_vars(as_vector = FALSE)
```

## Arguments

- as_vector:

  Logical. If TRUE, returns a named vector with column names for the
  meteorological variables. If FALSE, returns a data frame with v
  ariable names, column names and units. Default is FALSE.

## Value

Named vector with column names for the meteorological variables. The
names of the vector are the variable names in the Aeme object and the
values are the corresponding column names in the input data frame.

## Examples

``` r
get_met_vars()
#>      var_aeme               name_text  units
#> 1        Date                    Time       
#> 2  MET_radswd     Shortwave radiation   W/m2
#> 3  MET_radlwd      Longwave radiation   W/m2
#> 4  MET_cldcvr             Cloud cover      1
#> 5  MET_tmpair         Air temperature   degC
#> 6  MET_tmpdew    Dewpoint temperature   degC
#> 7  MET_prsttn        Station pressure     Pa
#> 8  MET_prmslp Mean sea level pressure     Pa
#> 9  MET_prvapr         Vapour pressure    hPa
#> 10 MET_humrel       Relative humidity      %
#> 11 MET_wndspd              Wind speed    m/s
#> 12 MET_wnddir          Wind direction degree
#> 13 MET_wnduvu            u wind speed    m/s
#> 14 MET_wnduvv            v wind speed    m/s
#> 15 MET_pprain                    Rain     mm
#> 16 MET_ppsnow                    Snow     mm
```
