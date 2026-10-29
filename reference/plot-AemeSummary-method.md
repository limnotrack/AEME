# Plot a variable from an AemeSummary

Plot a variable from an AemeSummary

## Usage

``` r
# S4 method for class 'AemeSummary'
plot(x, y, ...)
```

## Arguments

- x:

  An `AemeSummary`.

- y:

  character; the variable (`var_aeme`) to plot. Default: the first.

- ...:

  Unused.

## Value

A ggplot2 object: one panel per model, a line per layer, with a min-max
ribbon when the summary has ensemble statistics.
