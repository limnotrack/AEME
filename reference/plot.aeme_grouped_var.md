# Plot a grouped variable as a line plot, coloured by its non-time dimensions (e.g. one line per zone)

A thin wrapper around
[`as.data.frame.aeme_grouped_var()`](https://limnotrack.com/reference/as.data.frame.aeme_grouped_var.md)
that plots the resulting long-format data frame with ggplot2 – one
coloured line per combination of the variable's dimensions other than
`"time"` (e.g. one line per sediment zone for a GLM-AED `(nzones, time)`
AED flux variable).

## Usage

``` r
# S3 method for class 'aeme_grouped_var'
plot(x, var_sim = NULL, ylim = NULL, ...)
```

## Arguments

- x:

  an `aeme_grouped_var` object (see
  [`new_grouped_var()`](https://limnotrack.com/reference/new_grouped_var.md)).

- var_sim:

  character; variable name, used for the plot title/y-axis label.
  Default `NULL` (no title/label).

- ylim:

  numeric vector of length 2; y-axis limits. Default `NULL` (ranged to
  the data).

- ...:

  unused.

## Value

A ggplot2 object (the long-format data frame instead, with a warning, if
`x` has no `"time"` dimension).
