# Add observations to Aeme object

Add observations to Aeme object

## Usage

``` r
add_obs(aeme, lake = NULL, level = NULL)
```

## Arguments

- aeme:

  Aeme object.

- lake:

  data frame with required columns "Date", "var_aeme", "depth" and
  "value", and optional columns "depth_to" (bottom of an integrated
  sample) and "sd" (measurement standard deviation, in the variable's
  units). The legacy "depth_from" / "depth_to" column pair is accepted
  and collapsed to "depth" (with a one-time deprecation warning). "Date"
  may be a `Date`, `POSIXct` or date/datetime string; it is stored as a
  UTC `POSIXct` with daily rows anchored at 12:00:00. If NULL, no
  observations are added.

- level:

  data frame with columns "Date", "var_aeme" and "value". "Date" is
  handled as for `lake`. If NULL, no observations are added.

## Value

Aeme object with observations added
