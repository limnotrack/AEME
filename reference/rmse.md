# Root Mean Square Error (RMSE) fit function

Calculates the root mean square error between observed and modelled
values:

## Usage

``` r
rmse(obs, sim, na.rm = TRUE)
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

numeric; root mean square error, in the same units as `obs`/`sim`.

## Details

\$\$\text{RMSE} = \sqrt{\frac{1}{n} \sum\_{i=1}^n (\text{sim}\_i -
\text{obs}\_i)^2}\$\$

Already `0` (perfect fit) at its best and increasing as fit worsens, so
it is minimise-oriented as-is and needs no `_loss` companion

- pass it straight into `FUN_list`, which `calib_aeme` and `run_and_fit`
  minimise.

Like [`mae`](https://limnotrack.com/reference/mae.md), RMSE stays in the
variable's native units and so is not directly comparable across
variables with different units or magnitudes when combined in a
multi-variable `FUN_list` - see
[`mae`](https://limnotrack.com/reference/mae.md) for details. RMSE
additionally squares errors before averaging, so - like NSE - it weights
large deviations more heavily than MAE does.

## Examples

``` r
obs <- c(1, 2, 3, 4)
sim <- c(1.1, 2.1, 2.9, 4.2)
rmse(obs, sim)
#> [1] 0.1322876
```
