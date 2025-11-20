# Get derived variables needed for simulation

Get derived variables needed for simulation

## Usage

``` r
get_deriv_inputs(vars_sim)
```

## Arguments

- vars_sim:

  Character vector of variable names being simulated

## Value

Character vector of variable names that are needed as inputs

## Examples

``` r
get_deriv_inputs(vars_sim  = c("HYD_thmcln", "HYD_epidep", "CHM_oxyepi", "LKE_tli4"))
#> [1] "HYD_temp"   "HYD_epidep" "CHM_oxy"    "LKE_photic" "PHS_tp"    
#> [6] "NIT_tn"     "PHY_tchla" 
```
