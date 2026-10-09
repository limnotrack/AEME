# Percent Bias (PBIAS) fit function

Calculates the absolute percent bias between observed and modelled
values - the average tendency of the modelled values to be larger
(positive bias) or smaller (negative bias) than the observed values,
expressed as a percentage of the observed total:

## Usage

``` r
pbias(obs, sim, na.rm = TRUE)
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

numeric; absolute percent bias. Returns `NA` if the sum of `obs` is
zero.

## Details

\$\$\text{PBIAS} = 100 \times \left\| \frac{\sum\_{i=1}^n
(\text{sim}\_i - \text{obs}\_i)}{\sum\_{i=1}^n \text{obs}\_i}
\right\|\$\$

Already `0` (perfect fit, no systematic bias) at its best and increasing
as fit worsens in *either* direction, so it is minimise-oriented as-is
and needs no `_loss` companion - pass it straight into `FUN_list`, which
`calib_aeme` and `run_and_fit` minimise. The absolute value is used
rather than the signed value, since an equally large over- or
under-estimate is an equally poor fit - minimising the signed value
would instead push the calibration towards the most negative
(under-estimating) bias possible.

Unlike
[`mae`](https://limnotrack.com/reference/mae.md)/[`rmse`](https://limnotrack.com/reference/rmse.md),
it is expressed as a percentage of the observed total rather than in the
variable's native units, which makes it directly comparable across
variables with different units or magnitudes - similar to
[`nse`](https://limnotrack.com/reference/nse.md)/[`kge`](https://limnotrack.com/reference/kge.md)
in that respect. On its own it only captures systematic
over/under-estimation, not timing, variability or shape - it is
typically combined with NSE or KGE (which are largely insensitive to a
consistent bias) rather than used alone. See Moriasi et al. (2007) for
commonly used PBIAS performance thresholds.

## Examples

``` r
obs <- c(1, 2, 3, 4)
sim <- c(1.1, 2.1, 2.9, 4.2)
pbias(obs, sim)
#> [1] 3
```
