# Get all variables to be simulated

Get all variables to be simulated

## Usage

``` r
get_vars_sim(aeme, model_controls)
```

## Arguments

- aeme:

  aeme; object.

- model_controls:

  dataframe; of configuration loaded from "model_controls.csv".

## Value

vector of variable names

## Examples

``` r
data("model_controls", package = "AEME")
get_vars_sim(model_controls)
#> Error: unable to find an inherited method for function 'configuration' for signature 'aeme = "data.frame"'
```
