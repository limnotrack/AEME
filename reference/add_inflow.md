# Set inflow data from Aeme object

Replace existing inflow data in an Aeme object with new inflow data. The
new inflow data can be provided as either a named list of data frames or
a single data frame containing an "inflow_id" column to identify
different inflows. If the original inflows include a "precip" inflow, it
will be retained in the updated inflows.

## Usage

``` r
add_inflow(aeme, inflow, inflow_id)
```

## Arguments

- aeme:

  Aeme object.

- inflow:

  A data frame containing inflow data. Must include columns "Date",
  "HYD_flow", "HYD_temp", and "CHM_salt". An "inflow_id" column can be
  included to identify the inflow or it can be provided separately. If
  multiple unique inflow_ids are present, an error will be raised.

- inflow_id:

  A string specifying the identifier for the inflow. If not provided,
  the function will look for an "inflow_id" column in the inflow data
  frame. If the column is present but contains multiple unique values,
  an error will be raised.

## Value

Aeme object with inflow added
