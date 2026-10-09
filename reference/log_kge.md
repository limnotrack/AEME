# Log-transformed Kling-Gupta Efficiency fit function

[`kge`](https://limnotrack.com/reference/kge.md) calculated on
[`log1p()`](https://rdrr.io/r/base/Log.html)-transformed observed and
modelled values, for skewed/concentration-type variables (e.g. oxygen,
chlorophyll, nutrients) where a few peak events would otherwise dominate
the fit.

## Usage

``` r
log_kge(obs, sim, na.rm = TRUE)
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

numeric; KGE calculated on `log1p(obs)`/`log1p(sim)`, with a maximum of
`1` (perfect fit).

## Details

\$\$\text{KGE}\_{\text{log}} = \text{KGE}(\log\_{1p}(\text{obs}),
\log\_{1p}(\text{sim}))\$\$

[`log1p()`](https://rdrr.io/r/base/Log.html) tolerates zeros but not
negative values - not suitable for variables that can be negative.
Returned in its conventional orientation where **higher is better** (`1`
= perfect fit).

`calib_aeme()` and `run_and_fit()` **minimise** the values returned by
`FUN_list` entries - use
[`log_kge_loss`](https://limnotrack.com/reference/fit_loss.md) (which
returns `-1 * log_kge(obs, sim)`) as a calibration objective.

## See also

[`log_kge_loss`](https://limnotrack.com/reference/fit_loss.md) for the
minimise-oriented variant used in `FUN_list`.

## Examples

``` r
obs <- c(1, 2, 3, 4)
sim <- c(1.1, 2.1, 2.9, 4.2)
log_kge(obs, sim)
#> [1] 0.9622493
```
