# Read GOTM flux output

Read GOTM flux output

## Usage

``` r
read_gotm_flux_output(nc = NULL, file, dates = NULL, date_index = NULL)
```

## Arguments

- nc:

  An object of class `ncdf4` (as returned by either function
  [`nc_open`](https://rdrr.io/pkg/ncdf4/man/nc_open.html) or function
  [`nc_create`](https://rdrr.io/pkg/ncdf4/man/nc_create.html)),
  indicating what file to read from.

- file:

  File path to netCDF file. Only used if `nc` is NULL.

- dates:

  Dates to extract. If NULL, extract all dates. Defaults to NULL.

- date_index:

  Date index to extract. If NULL, extract all dates. Defaults to NULL.

## Value

List with GOTM flux output variables
