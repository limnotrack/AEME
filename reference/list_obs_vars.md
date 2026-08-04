# Get observation variable names

Get observation variable names

## Usage

``` r
list_obs_vars(aeme)
```

## Arguments

- aeme:

  Aeme object.

## Value

Observation variable names vector

## Examples

``` r
aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
aeme <- readRDS(aeme_file)
list_obs_vars(aeme)
#>       Cyanobacteria    Dissolved oxygen            Salinity   Secchi disk depth 
#>         "PHY_cyano"           "CHM_oxy"          "CHM_salt"        "RAD_secchi" 
#>   Water temperature Total chlorophyll a Dissolved organic C           Phosphate 
#>          "HYD_temp"         "PHY_tchla"           "CAR_doc"           "PHS_frp" 
#> Ammoniacal nitrogen             Nitrate      Total nitrogen    Total phosphorus 
#>           "NIT_amm"           "NIT_nit"            "NIT_tn"            "PHS_tp" 
#>         Water level 
#>        "LKE_lvlwtr" 
```
