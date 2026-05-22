# Get current precipitation status in Aeme object

This function checks whether precipitation is currently set as a
meteorological input or as an inflow in the Aeme object. It examines the
meteorological data for precipitation values and the inflow data for a
precipitation inflow.

## Usage

``` r
precip_status(aeme)
```

## Arguments

- aeme:

  Aeme object.

## Value

character. Either "precip_as_met", "precip_as_inflow" or "no_precip"
