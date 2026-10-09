# Normalized Mean Absolute Error (NMAE)

Calculates the Mean Absolute Error normalized by the mean of the
observed values:

## Usage

``` r
nmae(obs, sim, na.rm = TRUE)
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

numeric; NMAE (dimensionless). Returns `NA` if the mean of observations
is zero.

## Details

\$\$\text{NMAE} = \frac{\frac{1}{n} \sum\_{i=1}^n \|\text{sim}\_i -
\text{obs}\_i\|}{\bar{\text{obs}}}\$\$
