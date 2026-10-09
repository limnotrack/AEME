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

character. Either "met" if precipitation is set in meteorological data,
"inflow" if it is set as an inflow, or "none" if it is not set in
either.
