# Set simulation variables in model controls

Set simulation variables in model controls

## Usage

``` r
set_vars_sim(model_controls, vars_sim, simulate = TRUE, exclusive = FALSE)
```

## Arguments

- model_controls:

  dataframe; of configuration loaded from "model_controls.csv".

- vars_sim:

  character vector of variable names to set for simulation

- simulate:

  logical, whether to simulate the variables in vars_sim

- exclusive:

  logical, if TRUE, set all other variables to not simulate

## Value

Updated model_controls data frame

## Examples

``` r
model_controls <- data.frame(
 var_aeme = c("HYD_temp", "CHM_oxy", "PHS_tp"),
 simulate = c(TRUE, FALSE, TRUE),
 inf_default = c(NA, NA, NA),
 initial_wc = c(NA, NA, NA),
 initial_sed = c(NA, NA, NA),
 conversion_aed = c(1, 1, 1),
 stringsAsFactors = FALSE
)
vars_sim <- c("CHM_oxy", "NIT_tn", "PHS_tp")
updated_controls <- set_vars_sim(model_controls, vars_sim, simulate = TRUE)
#> ℹ Variables not found: `NIT_tn`.
#> Adding them to model_controls.
print(updated_controls)
#>   var_aeme simulate inf_default initial_wc initial_sed conversion_aed
#> 1  CHM_oxy     TRUE          NA         NA          NA              1
#> 2 HYD_temp     TRUE          NA         NA          NA              1
#> 3   NIT_tn     TRUE          NA         NA          NA              1
#> 4   PHS_tp     TRUE          NA         NA          NA              1
```
