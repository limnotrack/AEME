# Pearson Correlation Coefficient (r)

Calculates the linear correlation between simulated and observed values.

## Usage

``` r
r_pearson(obs, sim, na.rm = TRUE)
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

numeric; Pearson correlation coefficient ranging from -1 to 1.
