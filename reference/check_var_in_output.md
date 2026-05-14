# Check if a variable is present in the model output

Check if a variable is present in the model output

## Usage

``` r
check_var_in_output(aeme, model, var_sim, ens_n = 1)
```

## Arguments

- aeme:

  aeme; object.

- model:

  vector; of models to be used. Can be `dy_cd`, `glm_aed`, `gotm_wet`.

- var_sim:

  string; of variable to plot

- ens_n:

  numeric; ensemble number

## Value

A list with logical matrices indicating if each variable is present per
model, plus an overall `all_present` flag.
