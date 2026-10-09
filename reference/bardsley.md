# Bardsley Coefficient (B) fit function

Calculates the Bardsley coefficient between observed and modelled
values, combining the coefficient of determination (\\R^2\\) and
Nash-Sutcliffe Efficiency (\\NSE\\).

## Usage

``` r
bardsley(obs, sim, na.rm = TRUE)
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

numeric; Bardsley coefficient. Returns `NA` if undefined (e.g., zero
observed variance or singular model fit).

## Details

The Bardsley coefficient is calculated as: \$\$B = \frac{R^2}{2 -
\text{NSE}}\$\$

It provides a model performance metric that jointly accounts for
correlation and efficiency characteristics.

## See also

[`nse`](https://limnotrack.com/reference/nse.md) for the Nash-Sutcliffe
Efficiency component.

## Examples

``` r
obs <- c(1, 2, 3, 4)
sim <- c(1.1, 2.1, 2.9, 4.2)
bardsley(obs, sim)
#> [1] 0.9771887
```
