# Calculate the volume of a lake using bathymetry data or a hypsograph

Calculate the volume of a lake using bathymetry data or a hypsograph

## Usage

``` r
calc_lake_vol(aeme = NULL, hyps = NULL, depth = 0)
```

## Arguments

- aeme:

  aeme; object.

- hyps:

  data.frame with columns 'depth' and 'area'.

- depth:

  numeric. The depth to which to calculate the volume. If provided, the
  volume will be calculated to this depth. If not provided, the volume
  will be calculated to the maximum depth of the hypsograph.

## Value

numeric. The volume of the lake in cubic meters (m^3).

## Examples

``` r
aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
aeme <- readRDS(aeme_file)
vol <- calc_lake_vol(aeme)
```
