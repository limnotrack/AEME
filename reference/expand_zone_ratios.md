# Convert zone-ratio and zone-offset parameters to per-zone values

Expands the anchor + ratio parameterisation created by
[`zone_ratio_param()`](https://limnotrack.com/AEME/reference/zone_ratio_param.md)
(or the anchor + offset one from
[`zone_offset_param()`](https://limnotrack.com/AEME/reference/zone_offset_param.md))
into ordinary per-zone parameter rows that can be written to the model
configuration. The ratio or offset rows are removed and replaced with
one row per shallower zone, with `value[i] = value[i - 1] * ratio[i]` or
`value[i] = value[i - 1] + offset[i]`.

## Usage

``` r
expand_zone_ratios(param)
```

## Arguments

- param:

  data.frame of parameters, with the columns of
  [`parameters()`](https://limnotrack.com/AEME/reference/parameters.md).

## Value

data.frame of parameters with the same columns as `param` and no
zone-ratio or zone-offset rows.

## Details

[`input_model_parameters()`](https://limnotrack.com/AEME/reference/input_model_parameters.md)
calls this itself, so parameters with zone-ratio rows can be passed
straight to it, to
[`run_aeme_ensemble()`](https://limnotrack.com/AEME/reference/run_aeme_ensemble.md),
or to any function that writes parameters to the model. Call it directly
to see the per-zone values.

A `param` without any zone-ratio or zone-offset rows is returned
unchanged.

## See also

[`zone_ratio_param()`](https://limnotrack.com/AEME/reference/zone_ratio_param.md),
[`zone_offset_param()`](https://limnotrack.com/AEME/reference/zone_offset_param.md)

## Examples

``` r
p <- data.frame(model = "glm_aed", file = "aed.nml",
                name = "aed_sed_const2d/fsed_oxy", value = -30, min = -60,
                max = -5, group = NA_character_, index = 1)
p <- zone_ratio_param(p, n_zones = 3)
expand_zone_ratios(p)
#>     model    file                     name value min max group index
#> 1 glm_aed aed.nml aed_sed_const2d/fsed_oxy -30.0 -60  -5  <NA>     1
#> 2 glm_aed aed.nml aed_sed_const2d/fsed_oxy -15.0 -60  -5  <NA>     2
#> 3 glm_aed aed.nml aed_sed_const2d/fsed_oxy  -7.5 -60  -5  <NA>     3
```
