# Read DYRESM water level output

Read DYRESM water level output

## Usage

``` r
read_dy_wlev(nc = NULL, file)
```

## Arguments

- nc:

  An object of class `ncdf4` (as returned by either function
  [`nc_open`](https://rdrr.io/pkg/ncdf4/man/nc_open.html) or function
  [`nc_create`](https://rdrr.io/pkg/ncdf4/man/nc_create.html)),
  indicating what file to read from.

- file:

  File path to netCDF file. Only used if `nc` is NULL.

## Value

Data frame with Date and LKE_lvlwtr columns
