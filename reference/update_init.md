# Update initial conditions in AEME object based on lake observations.

Update initial conditions in AEME object based on lake observations.

## Usage

``` r
update_init(aeme, model_controls = NULL)
```

## Arguments

- aeme:

  Aeme object.

- model_controls:

  data.frame; model configuration, typically loaded via
  [`get_model_controls()`](https://limnotrack.com/reference/get_model_controls.md).

## Value

Aeme object with updated initial conditions
