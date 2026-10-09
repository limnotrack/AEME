# Plot a spin-up assessment

The chosen summary metric at the start of the analysis period as a
function of spin-up length, one line per reported variable.

## Usage

``` r
plot_spin_up(x, metric = NULL, ...)
```

## Arguments

- x:

  an `aeme_spin_up` object from
  [`assess_spin_up()`](https://limnotrack.com/reference/assess_spin_up.md).

- metric:

  character(1); `"spread"`, `"spread_cv"`, `"drift"` or `"drift_cv"`.
  Defaults to the metric the assessment was run with (`x$metric`).

- ...:

  unused.

## Value

A `ggplot` object.

## See also

[`assess_spin_up()`](https://limnotrack.com/reference/assess_spin_up.md)
