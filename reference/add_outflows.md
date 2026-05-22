# Add outflows to Aeme object

Add outflows to Aeme object

## Usage

``` r
add_outflows(aeme, data = NULL, elevation = NULL, factor = NULL)
```

## Arguments

- aeme:

  Aeme object.

- data:

  named list with data frames for each outflow. Each data frame must
  have columns "Date", "outflow". If NULL, no outflows are added.

- elevation:

  named list with elevation of each outflow. If NULL, no outflows are
  added. If elevation is -1, the outflow is assumed to be at the same
  elevation as the hypsograph. If elevation is not -1, it must be within
  the range of the hypsograph elevation.

- factor:

  named list with scaling factors to apply to outflows per model. If
  NULL, no scaling is applied.

## Value

Aeme object with outflows added
