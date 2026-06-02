# Get water balance components from AEME object

Get water balance components from AEME object

## Usage

``` r
get_wbal_components(aeme, model, remove_spin_up = FALSE, cumulative = FALSE)
```

## Arguments

- aeme:

  Aeme object.

- model:

  character vector; models to use. One or more of `"dy_cd"`,
  `"glm_aed"`, `"gotm_wet"`. Defaults to all models if not found in
  `aeme`.

- remove_spin_up:

  logical; remove spin-up period from plot. Default is TRUE.

- cumulative:

  logical; if TRUE, return cumulative sum of variable

## Value

List with observed lake levels, AEME water balance, and model components
