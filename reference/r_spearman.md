# Spearman Rank Correlation Coefficient (rs)

Calculates the monotonic relationship between simulated and observed
values using Spearman's rank correlation. It is more robust to outliers
than Pearson.

## Usage

``` r
r_spearman(obs, sim, na.rm = TRUE)
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

numeric; Spearman correlation coefficient ranging from -1 to 1.
