# Parameterise zoned parameters as an anchor value plus offsets

The additive counterpart of
[`zone_ratio_param()`](https://limnotrack.com/AEME/reference/zone_ratio_param.md),
for quantities such as sediment temperature where a ratio is not
meaningful (a ratio of degrees Celsius is arbitrary, and shallower zones
are normally *warmer* than deeper ones). Zone 1 (the deepest) is
calibrated directly and each shallower zone is the next deeper zone plus
an offset: `zone2 = zone1 + offset2`, `zone3 = zone2 + offset3`. With
`lower >= 0` the values can only increase towards the surface; use a
negative `lower` (or `upper <= 0`) to allow or force the opposite.

## Usage

``` r
zone_offset_param(param, n_zones, lower = 0, upper = 5, start = 1)
```

## Arguments

- param:

  data.frame of parameters for a single zoned parameter. Either one row
  (the zone 1 value) or one row per zone. The row with `index == 1` (or
  the first row if `index` is missing) is the anchor.

- n_zones:

  integer; number of zones in the model for this parameter.

- lower, upper:

  numeric; bounds for the offsets, in the units of the parameter.

- start:

  numeric; starting value for the offsets when `param` has a single row.
  If `param` has one row per zone, starting offsets are derived from the
  zone values and clipped to the bounds.

## Value

data.frame with the anchor row (`index = 1`) followed by `n_zones - 1`
offset rows.

## Details

Offset rows are named `<name>_zoffset` and are converted back to
per-zone values by
[`expand_zone_ratios()`](https://limnotrack.com/AEME/reference/expand_zone_ratios.md).

## See also

[`zone_ratio_param()`](https://limnotrack.com/AEME/reference/zone_ratio_param.md),
[`expand_zone_ratios()`](https://limnotrack.com/AEME/reference/expand_zone_ratios.md)

## Examples

``` r
p <- data.frame(model = "glm_aed", file = "glm4.nml",
                name = "sediment/sed_temp_mean", value = 12, min = 5,
                max = 25, group = NA_character_, index = 1)
zone_offset_param(p, n_zones = 3, lower = 0, upper = 5, start = 1)
#>     model     file                           name value min max group index
#> 1 glm_aed glm4.nml         sediment/sed_temp_mean    12   5  25  <NA>     1
#> 2 glm_aed glm4.nml sediment/sed_temp_mean_zoffset     1   0   5  <NA>     2
#> 3 glm_aed glm4.nml sediment/sed_temp_mean_zoffset     1   0   5  <NA>     3
```
