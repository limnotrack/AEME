# Get date index for each model in the AEME object

Get date index for each model in the AEME object

## Usage

``` r
get_date_index(aeme, model, remove_spin_up = TRUE)
```

## Arguments

- aeme:

  aeme; object.

- model:

  vector; of models to be used. Can be \`dy_cd\`, \`glm_aed\`,
  \`gotm_wet\`.

- remove_spin_up:

  logical; remove spin-up period from plot. Default is TRUE.

## Value

A list with date index for each model
