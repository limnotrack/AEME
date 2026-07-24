# Get modeled observation variables

Get modeled observation variables

## Usage

``` r
get_mod_obs_vars(aeme, model)
```

## Arguments

- aeme:

  Aeme object.

- model:

  character vector; models to use. One or more of `"dy_cd"`,
  `"glm_aed"`, `"gotm_wet"`, `"simstrat_aed2"`. Defaults to all models
  if not found in `aeme`.

## Value

Data frame with modeled observation variables and summary statistics.
