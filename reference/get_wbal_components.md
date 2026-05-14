# Get water balance components from AEME object

Get water balance components from AEME object

## Usage

``` r
get_wbal_components(aeme, model, remove_spin_up = TRUE, cumulative = FALSE)
```

## Arguments

- aeme:

  aeme; object.

- model:

  vector; of models to be used. Can be `dy_cd`, `glm_aed`, `gotm_wet`.

- remove_spin_up:

  logical; remove spin-up period from plot. Default is TRUE.

- cumulative:

  logical; if TRUE, return cumulative sum of variable

## Value

List with observed lake levels, AEME water balance, and model components
