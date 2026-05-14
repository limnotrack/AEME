# Generate a hypsograph curve

This function generates a hypsograph curve for a lake based on the
maximum depth, surface area, and volume development factor.

## Usage

``` r
generate_hypsograph(
  aeme = NULL,
  max_depth,
  surface_area,
  mean_depth,
  volume_development = NULL,
  elev = NULL,
  z_range = 0.2,
  ext_elev = 0
)
```

## Arguments

- aeme:

  An AEME object. If not provided, then max_depth and surface_area must
  be provided.

- max_depth:

  The maximum depth of the lake.

- surface_area:

  The surface area of the lake.

- mean_depth:

  The mean depth of the lake.

- volume_development:

  The volume development factor. Default is 3 \* mean_depth / max_depth.
  However

- elev:

  The elevation of the lake. If not provided, it is assumed to be the
  same as the maximum depth.

- z_range:

  numeric; 0-1, representing fraction of hypsograph to be used for
  extrapolation. Default is 0.2, which uses the top 20% of the
  hypsograph for extrapolation.

- ext_elev:

  numeric; metres to extend the hypograph by.

## Value

A data frame with columns for elevation, depth, and area.
