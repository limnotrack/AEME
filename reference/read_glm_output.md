# Read GLM netCDF output

Read GLM netCDF output

## Usage

``` r
read_glm_output(
  nc = NULL,
  vars_sim = NULL,
  depths = NULL,
  dates = NULL,
  date_index = NULL,
  incl_fluxes = TRUE,
  output_hour = 0,
  file,
  phyto_pars = NULL,
  load_all = TRUE,
  raw_output = FALSE
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

- load_all:

  logical; also load every other variable present in the netCDF file,
  beyond the declared `vars_sim` set. Each such variable is keyed by its
  AEME `var_aeme` name if
  [key_naming](https://limnotrack.com/reference/key_naming.md) has a
  translation for it, or by its raw GLM/netCDF name otherwise. Variables
  shaped like GLM's usual `(time)` or `(z, time)` output are loaded the
  same way as any declared variable; variables with other dimensions
  (e.g. `nzones`, `particle`, `sed_layers`, `lon`, `lat`) are loaded as
  a
  [`new_grouped_var()`](https://limnotrack.com/reference/new_grouped_var.md)
  object instead of being forced into the depth x time convention.
  Default `TRUE`.

- raw_output:

  logical; if `TRUE`, return output as close to the raw netCDF file as
  possible instead of AEME's standardised format: `(z, time)` variables
  are left on GLM's own, time-varying model layer midpoints (no
  interpolation onto a common depth grid), variables requested via
  `vars_sim` are keyed by their raw GLM/netCDF name (e.g. `"temp"`)
  rather than the translated AEME `var_aeme` name (e.g. `"HYD_temp"`),
  and no AED unit-conversion factors are applied. `depths` must not be
  supplied when `raw_output = TRUE`, since raw output has no common
  depth grid to interpolate onto. Default `FALSE`.

## Value

List with AEME output variables. Also includes `z`, GLM's own raw
layer-height matrix (height of each layer's top boundary above the lake
bottom, per timestep, before conversion to `LKE_depths`) – unlike
GOTM-WET/Simstrat-AED2, GLM's layer structure genuinely changes over
time, so this is kept alongside the derived depth grid rather than
discarded after use.
