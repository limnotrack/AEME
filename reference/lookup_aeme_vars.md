# View AEME variables

View AEME variables

## Usage

``` r
lookup_aeme_vars(group = NULL, var_aeme = NULL, name = NULL)
```

## Arguments

- group:

  character string to filter variables by group. Default is NULL.
  Options include "HYD" (hydrodynamic), "LKE" (Lake), "RAD" (Radiation),
  "CHM" (Chemistry), "PHS" (Phosphorus), "NIT" (Nitrogen), "CAR"
  (Carbon), "SIL" (Silica), "MET" (Meteorologica), "ZOO" (Zooplankton),
  "PHY" (Phytoplankton), "MAC" (Macrophytes), "NCS" (Non-conservative
  substances).

- var_aeme:

  character vector of exact AEME variable names to filter by (e.g.,
  "HYD_temp", "CHM_oxy", "PHS_tp"). Default is NULL.

- name:

  character string to filter variables by partial match in either the
  variable name or descriptive text. Default is NULL.

## Value

A data frame with AEME variables

## Examples

``` r
# Filter by group keyword in var_aeme
lookup_aeme_vars(group = "NIT")
#>   var_aeme                    text units
#> 1  NIT_amm     Ammoniacal nitrogen g/m^3
#> 2  NIT_don     Dissolved organic N g/m^3
#> 3 NIT_donr                              
#> 4  NIT_nit                 Nitrate g/m^3
#> 5  NIT_pin Particulate inorganic N g/m^3
#> 6  NIT_pon   Particulate organic N g/m^3
#> 7 NIT_ponr                              
#> 8   NIT_tn          Total nitrogen g/m^3

# Filter by exact variable name
lookup_aeme_vars(var_aeme = "NIT_tn")
#>   var_aeme           text units
#> 1   NIT_tn Total nitrogen g/m^3

# Filter by partial name or text match
lookup_aeme_vars(name = "chlorophyll")
#>      var_aeme                                text  units
#> 1    LKE_tlic   Trophic Level Index Chlorophyll-a      1
#> 2 PHY_rstchla Remotely sensed total chlorophyll a mg/m^3
#> 3   PHY_tchla                 Total chlorophyll a mg/m^3

# Combine filters
lookup_aeme_vars(group = "NIT", var_aeme = "HYD_temp", name = "phosphate")
#>    var_aeme                    text units
#> 1  HYD_temp       Water temperature  degC
#> 2   NIT_amm     Ammoniacal nitrogen g/m^3
#> 3   NIT_don     Dissolved organic N g/m^3
#> 4  NIT_donr                              
#> 5   NIT_nit                 Nitrate g/m^3
#> 6   NIT_pin Particulate inorganic N g/m^3
#> 7   NIT_pon   Particulate organic N g/m^3
#> 8  NIT_ponr                              
#> 9    NIT_tn          Total nitrogen g/m^3
#> 10  PHS_frp               Phosphate g/m^3
```
