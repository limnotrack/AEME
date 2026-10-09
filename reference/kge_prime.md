# Modified Kling-Gupta Efficiency (KGE') fit function

As [`kge`](https://limnotrack.com/reference/kge.md), but replaces the
raw variability ratio with a coefficient-of-variation ratio (Kling et
al. 2012):

## Usage

``` r
kge_prime(obs, sim, na.rm = TRUE)
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

numeric; KGE', with a maximum of `1` (perfect fit). Returns `NA` if
undefined.

## Details

\$\$\text{KGE}' = 1 - \sqrt{(r - 1)^2 + (\gamma - 1)^2 + (\beta -
1)^2}\$\$

where the coefficient-of-variation ratio (\\\gamma\\) decouples the
variability term from the bias term more cleanly:

\$\$\gamma = \frac{\sigma\_{\text{sim}} /
\mu\_{\text{sim}}}{\sigma\_{\text{obs}} / \mu\_{\text{obs}}}\$\$

This is generally the recommended default over the original KGE.
Returned in its conventional orientation where **higher is better** (`1`
= perfect fit).

`calib_aeme()` and `run_and_fit()` **minimise** the values returned by
`FUN_list` entries - use
[`kge_prime_loss`](https://limnotrack.com/reference/fit_loss.md) (which
returns `-1 * kge_prime(obs, sim)`) as a calibration objective.

## See also

[`kge_prime_loss`](https://limnotrack.com/reference/fit_loss.md) for the
minimise-oriented variant used in `FUN_list`.

## Examples

``` r
obs <- c(1, 2, 3, 4)
sim <- c(1.1, 2.1, 2.9, 4.2)
kge_prime(obs, sim)
#> [1] 0.9661881
```
