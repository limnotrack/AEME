# Mean Absolute Error (MAE) fit function

Calculates the mean absolute error between observed and modelled values:

## Usage

``` r
mae(obs, sim, na.rm = TRUE)
```

## Arguments

- obs:

  Numeric vector of observed values.

- sim:

  Numeric vector of simulated (modelled) values.

- na.rm:

  Logical; should missing values (NA) in either vector be removed
  pairwise before computation? Default is `TRUE`.

## Value

numeric; mean absolute error, in the same units as `obs`/`sim`.

## Details

\$\$\text{MAE} = \frac{1}{n} \sum\_{i=1}^n \|\text{sim}\_i -
\text{obs}\_i\|\$\$

Already `0` (perfect fit) at its best and increasing as fit worsens, so
it is minimise-oriented as-is and needs no `_loss` companion - pass it
straight into `FUN_list`, which `calib_aeme` and `run_and_fit` minimise.

MAE stays in the variable's native units, so it is **not** directly
comparable across variables with different units or magnitudes - summing
MAE from, say, a temperature fit (degC) and an oxygen fit (mg/L) in a
multi-variable `FUN_list` lets whichever variable happens to have the
larger natural magnitude dominate the combined fit. Prefer a
dimensionless metric such as
[`nse_loss`](https://limnotrack.com/reference/fit_loss.md) or
[`kge_loss`](https://limnotrack.com/reference/fit_loss.md) when
combining fits across variables; MAE/RMSE are more suited to
single-variable calibration or to reporting fit in interpretable,
original units.

## Examples

``` r
obs <- c(1, 2, 3, 4)
sim <- c(1.1, 2.1, 2.9, 4.2)
mae(obs, sim)
#> [1] 0.125
```
