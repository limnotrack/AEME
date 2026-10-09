# Nash-Sutcliffe Efficiency (NSE) fit function

Calculates the Nash-Sutcliffe Efficiency between observed and modelled
values, in its conventional orientation where **higher is better** (`1`
= perfect fit).

## Usage

``` r
nse(obs, sim, na.rm = TRUE)
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

numeric; NSE, ranging `-Inf` to `1` (`1` = perfect fit). Returns `NA` if
undefined (e.g., zero observed variance).

## Details

NSE is calculated as: \$\$\text{NSE} = 1 - \frac{\sum\_{i=1}^n
(\text{obs}\_i - \text{sim}\_i)^2}{\sum\_{i=1}^n (\text{obs}\_i -
\bar{\text{obs}})^2}\$\$

It ranges from `-Inf` to `1` (`1` = perfect fit, `0` = no better than
the mean of the observations). It is dimensionless, which makes it a
reasonable default when combining fit values across variables with
different units or magnitudes - but it is squared-error based, so it
over-weights peaks and under-weights errors in the low/baseline range,
and it conflates bias, variability and timing error into one number. See
[`kge`](https://limnotrack.com/reference/kge.md)/[`kge_prime`](https://limnotrack.com/reference/kge_prime.md)
for a fit function that keeps those separate.

`calib_aeme()` and `run_and_fit()` **minimise** the values returned by
`FUN_list` entries, so `nse()` is not suitable as a calibration
objective directly - use
[`nse_loss`](https://limnotrack.com/reference/fit_loss.md) (which
returns `-1 * nse(obs, sim)`) for that.

## See also

[`nse_loss`](https://limnotrack.com/reference/fit_loss.md) for the
minimise-oriented variant used in `FUN_list`.

## Examples

``` r
obs <- c(1, 2, 3, 4)
sim <- c(1.1, 2.1, 2.9, 4.2)
nse(obs, sim)
#> [1] 0.986
```
