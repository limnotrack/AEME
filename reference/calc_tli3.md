# Calculate TLI 3

The TLI protocol (Burns et al.) uses annual averages of each variable,
which deseasonalises the data. Supply annual means, not individual
samples.

## Usage

``` r
calc_tli3(chla, tn, tp)
```

## Arguments

- chla:

  Chlorophyll-a concentration in µg/L

- tn:

  Total nitrogen concentration in mg/L

- tp:

  Total phosphorus concentration in mg/L

## Value

TLI 3
