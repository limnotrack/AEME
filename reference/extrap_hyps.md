# Extend hypsometry to a greater elevation using linear extrapolation

Extend hypsometry to a greater elevation using linear extrapolation

## Usage

``` r
extrap_hyps(hypsograph, z_range = 0.2, ext_elev)
```

## Arguments

- hypsograph:

  dataframe; with hypsograph

- z_range:

  numeric; 0-1, representing fraction of hypsograph to be used for
  extrapolation. Default is 0.2, which uses the top 20% of the
  hypsograph for extrapolation.

- ext_elev:

  numeric; metres to extend the hypograph by.

## Value

dataframe with extrapolated hypsmetry
