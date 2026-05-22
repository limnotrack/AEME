# Add inflows to Aeme object

Add inflows to Aeme object

## Usage

``` r
add_inflows(aeme, data)
```

## Arguments

- aeme:

  Aeme object.

- data:

  list with data frames for each inflow. Each data frame must have
  columns "Date", "HYD_flow", "HYD_temp" and "CHM_salt". If NULL, no
  inflows are added.

## Value

Aeme object with inflows added
