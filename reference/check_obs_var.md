# Check if a variable is present in the observations for the model time period

Check if a variable is present in the observations for the model time
period

## Usage

``` r
check_obs_var(aeme, var_sim)
```

## Arguments

- aeme:

  aeme; object.

- var_sim:

  character; variable in the AEME format (e.g. "HYD_temp").

## Value

A list with two elements:

- `obs`: A data frame with the following columns:

  - `model`: Model name

  - `var_aeme`: Variable name

  - `n`: Number of observations

- `vars_present`: A character vector of variables present in the
  observations
