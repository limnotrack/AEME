# Check if a variable is present in the model output

Check if a variable is present in the model output

## Usage

``` r
check_var_in_output(aeme, model, var_sim, ens_n = 1)
```

## Arguments

- aeme:

  Aeme object.

- model:

  character vector; models to use. One or more of `"dy_cd"`,
  `"glm_aed"`, `"gotm_wet"`, `"simstrat_aed2"`. Defaults to all models
  if not found in `aeme`.

- var_sim:

  string; of variable to plot

- ens_n:

  numeric; ensemble number

## Value

A list with logical matrices indicating if each variable is present per
model, plus an overall `all_present` flag.
