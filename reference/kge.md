# Kling-Gupta Efficiency (KGE) fit function

Calculates the Kling-Gupta Efficiency (Gupta et al. 2009) between
observed and modelled values, in its conventional orientation where
**higher is better** (`1` = perfect fit).

## Usage

``` r
kge(obs, sim, na.rm = TRUE)
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

numeric; KGE, with a maximum of `1` (perfect fit). Returns `NA` if
undefined (e.g., zero observed variance or mean).

## Details

KGE decomposes fit into three components instead of conflating them the
way [`nse`](https://limnotrack.com/reference/nse.md)'s single
squared-error term does:

\$\$\text{KGE} = 1 - \sqrt{(r - 1)^2 + (\alpha - 1)^2 + (\beta -
1)^2}\$\$

where the variability ratio (\\\alpha\\) and bias ratio (\\\beta\\) are
calculated as:

\$\$\alpha = \frac{\sigma\_{\text{sim}}}{\sigma\_{\text{obs}}}, \quad
\beta = \frac{\mu\_{\text{sim}}}{\mu\_{\text{obs}}}\$\$

with \\r\\ being the Pearson correlation, \\\sigma\\ the standard
deviation, and \\\mu\\ the mean. Maximum is `1` (perfect fit). Being
dimensionless like NSE, it is also a reasonable choice when combining
fit values across variables with different units or magnitudes, and is
generally preferred over NSE in current hydrological/environmental
modelling practice.

`calib_aeme()` and `run_and_fit()` **minimise** the values returned by
`FUN_list` entries - use
[`kge_loss`](https://limnotrack.com/reference/fit_loss.md) (which
returns `-1 * kge(obs, sim)`) as a calibration objective.

## See also

[`kge_loss`](https://limnotrack.com/reference/fit_loss.md) for the
minimise-oriented variant used in `FUN_list`.

## Examples

``` r
obs <- c(1, 2, 3, 4)
sim <- c(1.1, 2.1, 2.9, 4.2)
kge(obs, sim)
#> [1] 0.9663051
```
