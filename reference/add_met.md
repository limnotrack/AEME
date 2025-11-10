# Add meteorological data to Aeme object

Add meteorological data to Aeme object

## Usage

``` r
add_met(aeme, met)
```

## Arguments

- aeme:

  aeme; object.

- met:

  data frame with meteorological data. Must include columns "Date",
  "MET_radswd", "MET_radswd", "MET_pprain" and "MET_wndspd" or
  "MET_wnduvu" and "MET_wnduvv".

## Value

Aeme object with meteorological data added
