# Convert dissolved oxygen between mg/L and percent saturation

Convert dissolved oxygen between mg/L and percent saturation

## Usage

``` r
convert_do(
  value,
  depth,
  temp,
  baro,
  altitude = 0,
  salinity = 0,
  model = "garcia_benson",
  direction = "to_mgL"
)
```

## Arguments

- value:

  numeric vector of dissolved oxygen values to convert

- depth:

  depth (m) at which the DO measurement was made

- temp:

  water temperature (°C)

- baro:

  barometric pressure (mb)

- altitude:

  altitude (m). Only used if `baro` is missing.

- salinity:

  salinity (ppt). Default is 0 for freshwater.

- model:

  character, solubility model to use. Options are "garcia",
  "garcia_benson", "weiss", or "benson".

- direction:

  character, conversion direction. Options are "to_mgL" (percent
  saturation to mg/L) or "to_percent" (mg/L to percent saturation).

## Value

numeric vector of converted dissolved oxygen values
