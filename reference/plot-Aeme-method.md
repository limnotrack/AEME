# Plot method for Aeme objects

Plot an Aeme object

## Usage

``` r
# S4 method for class 'Aeme'
plot(x, y, ..., add = FALSE)
```

## Arguments

- x:

  An Aeme object.

- y:

  An Aeme slot name (character). Defaults to `"output"`.

- ...:

  Additional arguments (currently unused).

- add:

  Logical; add to current plot? (currently unused)

## Value

A ggplot object, or prints to the active graphics device.

## Details

This method plots the Aeme object.
