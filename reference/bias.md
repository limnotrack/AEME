# Mean bias fit function

Calculates the mean difference between modelled and observed values, in
the variable's native units:

## Usage

``` r
bias(obs, sim, na.rm = TRUE)
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

numeric; `mean(sim - obs)`, signed, in the same units as `obs`/`sim`.

## Details

\$\$\text{Bias} = \frac{1}{n} \sum\_{i=1}^n (\text{sim}\_i -
\text{obs}\_i)\$\$

Positive when the model overestimates on average, negative when it
underestimates. Unlike every other function in this file, the raw
(signed) value is returned rather than a zero-is-best, non-negative one.

**This makes `bias()` unsafe to drop directly into `FUN_list` for
calibration**: since `calib_aeme()`/`run_and_fit()` minimise the
returned value, minimising a signed bias would push the calibration
towards the most negative (maximally under-estimating) solution rather
than towards zero bias. Use
[`pbias`](https://limnotrack.com/reference/pbias.md) (or
`abs(bias(obs, sim))`) as a calibration objective; use `bias()` for
diagnosing the *direction* of a systematic error when inspecting fit
after the fact. Like
[`mae`](https://limnotrack.com/reference/mae.md)/[`rmse`](https://limnotrack.com/reference/rmse.md),
it stays in the variable's native units and so is not directly
comparable across variables with different units or magnitudes.

## Examples

``` r
obs <- c(1, 2, 3, 4)
sim <- c(1.1, 2.1, 2.9, 4.2)
bias(obs, sim)
#> [1] 0.075
```
