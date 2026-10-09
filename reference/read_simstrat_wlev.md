# Read Simstrat-AED2 lake water level output

Read Simstrat-AED2 lake water level output

## Usage

``` r
read_simstrat_wlev(nc = NULL, file, model = "simstrat_aed2")
```

## Arguments

- nc:

  An object of class `ncdf4` (as returned by either function
  [`nc_open`](https://rdrr.io/pkg/ncdf4/man/nc_open.html) or function
  [`nc_create`](https://rdrr.io/pkg/ncdf4/man/nc_create.html)),
  indicating what file to read from.

- file:

  File path to netCDF file. Only used if `nc` is NULL.

- model:

  character; which Simstrat coupling this output came from,
  `"simstrat_aed2"` (default) or `"simstrat_aed"`. Selects the matching
  `key_naming` column for variable-name translation – the netCDF file
  format itself (produced by
  [`write_simstrat_nc`](https://limnotrack.com/reference/write_simstrat_nc.md))
  is identical either way.

## Value

Data frame with Date and LKE_lvlwtr columns
