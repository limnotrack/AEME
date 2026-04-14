# Add derived output variables to model output

Add derived output variables to model output

## Usage

``` r
add_deriv_output(out_list, hyps, vars_sim = NULL)
```

## Arguments

- out_list:

  list of model output variables

- hyps:

  data frame with hypsograph data

- vars_sim:

  character vector of variable names to add. If NULL, all variables with
  registered functions will be added. If not NULL, only the specified
  variables will be added, but their dependencies will also be added.

## Value

List of model output variables with derived variables added
