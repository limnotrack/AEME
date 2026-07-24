# Read Simstrat-AED2 netCDF output

Reads the consolidated `output.nc` produced by
[`write_simstrat_nc`](https://limnotrack.com/reference/write_simstrat_nc.md)
(Simstrat itself writes one text `.dat` file per variable; AEME converts
these to netCDF as a post-processing step so the same reading approach
as GLM-AED/GOTM-WET can be reused).

## Usage

``` r
read_simstrat_output(
  nc = NULL,
  vars_sim = NULL,
  depths = NULL,
  dates = NULL,
  date_index = NULL,
  incl_fluxes = TRUE,
  output_hour = 0,
  file,
  phyto_pars = NULL
)
```

## Arguments

- nc:

  An object of class `ncdf4` (as returned by either function
  [`nc_open`](https://rdrr.io/pkg/ncdf4/man/nc_open.html) or function
  [`nc_create`](https://rdrr.io/pkg/ncdf4/man/nc_create.html)),
  indicating what file to read from.

- vars_sim:

  Variables to extract in the AEME format e.g. "HYD_temp"

- depths:

  Depths to extract. If NULL, extract all model layer depths. Defaults
  to NULL.

- dates:

  Dates to extract. If NULL, extract all dates. Defaults to NULL.

- date_index:

  Date index to extract. If NULL, extract all dates. Defaults to NULL.

- incl_fluxes:

  Logical indicating whether to include flux variables. Defaults to
  TRUE.

- output_hour:

  Hour of the day to extract (0-23). Defaults to 0.

- file:

  File path to netCDF file. Only used if `nc` is NULL.

- phyto_pars:

  Data frame with phytoplankton parameters from AED.

## Value

List with AEME output variables
