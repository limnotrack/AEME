# Minimise-oriented (loss) variants of NSE / KGE fit functions

[`nse`](https://limnotrack.com/reference/nse.md),
[`kge`](https://limnotrack.com/reference/kge.md),
[`kge_prime`](https://limnotrack.com/reference/kge_prime.md) and
[`log_kge`](https://limnotrack.com/reference/log_kge.md) return their
conventional statistic, where `1` is a perfect fit and higher is better.
`calib_aeme()` and `run_and_fit()` instead **minimise** the values
returned by `FUN_list` entries, so these `_loss` companions return
`-1 *` the corresponding statistic (lower is better, `-1` = perfect fit)
and are what you pass in `FUN_list` for calibration:

## Usage

``` r
nse_loss(obs, sim, na.rm = TRUE)

kge_loss(obs, sim, na.rm = TRUE)

kge_prime_loss(obs, sim, na.rm = TRUE)

log_kge_loss(obs, sim, na.rm = TRUE)
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

numeric; `-1 *` the corresponding statistic (`-1` = perfect fit, higher
= worse fit).

## Details

    FUN_list <- list(HYD_temp = kge_loss, LKE_lvlwtr = rmse)

[`mae`](https://limnotrack.com/reference/mae.md),
[`rmse`](https://limnotrack.com/reference/rmse.md) and
[`pbias`](https://limnotrack.com/reference/pbias.md) are already
`0`-is-best, minimise-oriented, so they have no `_loss` companion - use
them directly.

## See also

[`nse`](https://limnotrack.com/reference/nse.md),
[`kge`](https://limnotrack.com/reference/kge.md),
[`kge_prime`](https://limnotrack.com/reference/kge_prime.md),
[`log_kge`](https://limnotrack.com/reference/log_kge.md) for the
conventional (higher-is-better) statistics.

## Examples

``` r
obs <- c(1, 2, 3, 4)
sim <- c(1.1, 2.1, 2.9, 4.2)
nse_loss(obs, sim)
#> [1] -0.986
kge_loss(obs, sim)
#> [1] -0.9663051
kge_prime_loss(obs, sim)
#> [1] -0.9661881
log_kge_loss(obs, sim)
#> [1] -0.9622493
```
