# Get date index for each model in the AEME object

Get date index for each model in the AEME object

## Usage

``` r
get_date_index(aeme, model, remove_spin_up = TRUE)
```

## Arguments

- aeme:

  Aeme object.

- model:

  character vector; models to use. One or more of `"dy_cd"`,
  `"glm_aed"`, `"gotm_wet"`, `"simstrat_aed2"`. Defaults to all models
  if not found in `aeme`.

- remove_spin_up:

  logical; remove spin-up period from plot. Default is TRUE.

## Value

A list with date index for each model
