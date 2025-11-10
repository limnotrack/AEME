# Get observation variable names

Get observation variable names

## Usage

``` r
list_obs_vars(aeme)
```

## Arguments

- aeme:

  aeme; object.

## Value

Observation variable names vector

## Examples

``` r
aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
aeme <- readRDS(aeme_file)
list_obs_vars(aeme)
#>            Cyanobacteria         Dissolved oxygen                 Salinity 
#>              "PHY_cyano"                "CHM_oxy"               "CHM_salt" 
#>        Secchi disk depth        Water temperature      Total chlorophyll a 
#>             "RAD_secchi"               "HYD_temp"              "PHY_tchla" 
#> Dissolved organic carbon                Phosphate      Ammoniacal nitrogen 
#>                "CAR_doc"                "PHS_frp"                "NIT_amm" 
#>                  Nitrate           Total nitrogen         Total phosphorus 
#>                "NIT_nit"                 "NIT_tn"                 "PHS_tp" 
#>              Water level 
#>             "LKE_lvlwtr" 
```
