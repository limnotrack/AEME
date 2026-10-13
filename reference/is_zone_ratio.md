# Identify zone-ratio and zone-offset parameters

Zone-step parameters are the rows added by
[`zone_ratio_param()`](https://limnotrack.com/AEME/reference/zone_ratio_param.md)
and
[`zone_offset_param()`](https://limnotrack.com/AEME/reference/zone_offset_param.md):
their names end in `_zratio` or `_zoffset`.

## Usage

``` r
is_zone_ratio(param)

zone_base_name(x)
```

## Arguments

- param:

  data.frame with a `name` column.

- x:

  character vector of parameter names.

## Value

`is_zone_ratio()` returns a logical vector, one element per row of
`param`, that is `TRUE` for zone-ratio and zone-offset rows.
`zone_base_name()` returns `x` with the zone suffix removed, i.e. the
name of the anchor parameter the step belongs to; names without a suffix
are returned unchanged.

## See also

[`zone_ratio_param()`](https://limnotrack.com/AEME/reference/zone_ratio_param.md),
[`zone_offset_param()`](https://limnotrack.com/AEME/reference/zone_offset_param.md),
[`expand_zone_ratios()`](https://limnotrack.com/AEME/reference/expand_zone_ratios.md)

## Examples

``` r
p <- data.frame(model = "glm_aed", file = "aed.nml",
                name = "aed_sed_const2d/fsed_oxy", value = -30, min = -60,
                max = -5, group = NA_character_, index = 1)
p <- zone_ratio_param(p, n_zones = 3)
is_zone_ratio(p)
#> [1] FALSE  TRUE  TRUE
zone_base_name(p$name)
#> [1] "aed_sed_const2d/fsed_oxy" "aed_sed_const2d/fsed_oxy"
#> [3] "aed_sed_const2d/fsed_oxy"
```
