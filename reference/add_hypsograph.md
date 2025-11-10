# Add hypsograph to Aeme object

Add hypsograph to Aeme object

## Usage

``` r
add_hypsograph(
  aeme = NULL,
  hypsograph = NULL,
  surf_elev = 0,
  lake_depth = NULL,
  lake_area = NULL,
  ext_elev = 0
)
```

## Arguments

- aeme:

  aeme; object.

- hypsograph:

  data frame with columns "depth" and "area". Depth should be monotonic
  decreasing and area should be monotonic increasing. If elevation is
  not provided, it will be calculated as depth + lake elevation. If
  NULL, a simple hypsograph will be generated using lake depth and area.

- surf_elev:

  Lake surface elevation (m). Required if hypsograph is NULL.

- lake_depth:

  Lake maximum depth (m). Required if hypsograph is NULL.

- lake_area:

  Lake surface area (m2). Required if hypsograph is NULL.

- ext_elev:

  numeric; metres to extend the hypograph by.

## Value

Aeme object with hypsograph added

## Examples

``` r
depth <- seq(-5, 0, by = 0.5)
area <- c(0, 10, 30, 50, 70, 90, 110, 130, 150, 170, 190)
hypsograph <- data.frame(depth = depth, area = area)
```
