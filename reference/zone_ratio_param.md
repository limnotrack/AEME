# Parameterise zoned parameters as an anchor value plus ratios

Sediment zone parameters (e.g. `aed_sed_const2d/fsed_oxy`,
`sediment/sed_temp_mean`) are normally calibrated as independent values
per zone, which allows combinations that break the expected change with
depth (e.g. zone 1 oxygen flux greater than zone 2).
`zone_ratio_param()` replaces them with the value for zone 1 (the
deepest zone), which is calibrated as usual, and one ratio per shallower
zone. Each zone is a fraction of the next deeper zone, so
`zone2 = ratio2 * zone1` and `zone3 = ratio3 * zone2`, which guarantees
that a shallower zone never exceeds a deeper one (in magnitude) when the
ratios are within 0 and 1.

## Usage

``` r
zone_ratio_param(param, n_zones, lower = 0.1, upper = 1, start = 0.5)
```

## Arguments

- param:

  data.frame of parameters for a single zoned parameter. Either one row
  (the zone 1 value) or one row per zone. The row with `index == 1` (or
  the first row if `index` is missing) is the anchor.

- n_zones:

  integer; number of zones in the model for this parameter.

- lower, upper:

  numeric; bounds for the ratios.

- start:

  numeric; starting value for the ratios when `param` has a single row.
  If `param` has one row per zone, starting ratios are derived from the
  zone values and clipped to the bounds.

## Value

data.frame with the anchor row (`index = 1`) followed by `n_zones - 1`
ratio rows.

## Details

The ratio rows are ordinary calibration parameters (named
`<name>_zratio`). They are converted back to real per-zone values by
[`expand_zone_ratios()`](https://limnotrack.com/AEME/reference/expand_zone_ratios.md)
immediately before the model files are written –
[`input_model_parameters()`](https://limnotrack.com/AEME/reference/input_model_parameters.md)
does this itself – so no change to the model configuration is needed.

For quantities that do not scale sensibly as a ratio, such as sediment
temperature, use
[`zone_offset_param()`](https://limnotrack.com/AEME/reference/zone_offset_param.md).

## See also

[`zone_offset_param()`](https://limnotrack.com/AEME/reference/zone_offset_param.md),
[`expand_zone_ratios()`](https://limnotrack.com/AEME/reference/expand_zone_ratios.md)

## Examples

``` r
p <- data.frame(model = "glm_aed", file = "aed.nml",
                name = "aed_sed_const2d/fsed_oxy", value = -30, min = -60,
                max = -5, group = NA_character_, index = 1)
zone_ratio_param(p, n_zones = 3)
#>     model    file                            name value   min max group index
#> 1 glm_aed aed.nml        aed_sed_const2d/fsed_oxy -30.0 -60.0  -5  <NA>     1
#> 2 glm_aed aed.nml aed_sed_const2d/fsed_oxy_zratio   0.5   0.1   1  <NA>     2
#> 3 glm_aed aed.nml aed_sed_const2d/fsed_oxy_zratio   0.5   0.1   1  <NA>     3
```
