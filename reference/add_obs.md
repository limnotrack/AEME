# Add observations to Aeme object

Add observations to Aeme object

## Usage

``` r
add_obs(aeme, lake = NULL, level = NULL, depth_ref = c("surface", "bottom"))
```

## Arguments

- aeme:

  Aeme object.

- lake:

  data frame with required columns "Date", "var_aeme", "depth" and
  "value", and optional columns "depth_to" (bottom of an integrated
  sample), "sd" (measurement standard deviation, in the variable's
  units) and "depth_ref" (see `depth_ref`). The legacy "depth_from" /
  "depth_to" column pair is accepted and collapsed to "depth" (with a
  one-time deprecation warning). "Date" may be a `Date`, `POSIXct` or
  date/datetime string; it is stored as a UTC `POSIXct` with daily rows
  anchored at 12:00:00. If NULL, no observations are added.

- level:

  data frame with columns "Date", "var_aeme" and "value". "Date" is
  handled as for `lake`. If NULL, no observations are added.

- depth_ref:

  character; what "depth" in `lake` is measured from. `"surface"`
  (default): metres below the water surface. `"bottom"`: metres above
  the lake bed, for observations recorded as a height off the bottom (a
  logger 1 m above the sediment). A "depth_ref" column in `lake`, with
  values `"surface"` or `"bottom"`, sets this per row and takes
  precedence; `depth_ref` applies to rows without one.

## Value

Aeme object with observations added

## Details

Stored observations always have `depth` in metres below the surface, so
everything that reads it keeps working. For a bottom-referenced row the
height you give is kept in `height_above_bed` (with
`depth_ref = "bottom"`), and `depth` is worked out as `level - height`,
where `level` is the height of the water surface above the bed on that
date: from the lake level observations (those in `level`, or already on
the object), interpolated in time; failing that, the full depth of the
basin, with a warning because it ignores how far the real level was from
full. That `depth` is an estimate.
[`get_var()`](https://limnotrack.com/AEME/reference/get_var.md) with
`use_obs = TRUE` and
[`ensemble_summary()`](https://limnotrack.com/AEME/reference/ensemble_summary.md)
compare bottom-referenced observations with the model at
`height_above_bed` using the model's own water level, which is what "1 m
above the bed" means.
