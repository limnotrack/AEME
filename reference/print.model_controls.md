# Print a model controls table

Print a model controls table

## Usage

``` r
# S3 method for class 'model_controls'
print(x, all = FALSE, ...)
```

## Arguments

- x:

  a `model_controls` object (see
  [`get_model_controls()`](https://limnotrack.com/reference/get_model_controls.md)).

- all:

  logical; show every variable, including ones not set to simulate.
  Default `FALSE` shows only the simulated ones, which is almost always
  what you want to check.

- ...:

  unused.

## Value

`x`, invisibly.
