# Read model outputs and format to AEME standard

Read model outputs and format to AEME standard

## Usage

``` r
read_model_outputs(
  nc = NULL,
  lake_dir,
  model,
  vars_sim = NULL,
  depths = NULL,
  dates = NULL,
  date_index = NULL,
  incl_fluxes = TRUE,
  output_hour = 0,
  phyto_pars = NULL,
  load_all = TRUE,
  use_dat = NULL,
  daily_mean = FALSE
)
```

## Arguments

- nc:

  Open netCDF object. If NULL, will open netCDF from lake_dir. This is
  useful when reading multiple variables from the same file to avoid
  reopening the file multiple times. Defaults to NULL.

- lake_dir:

  Directory of lake model outputs

- model:

  Model name. One of "gotm_wet", "glm_aed", or "dy_cd".

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

- phyto_pars:

  Dataframe of phytoplankton parameters for GLM-AED model. See
  [`?read_glm_output`](https://limnotrack.com/reference/read_glm_output.md)
  for details. Defaults to NULL.

- load_all:

  logical; for `model = "glm_aed"`, also load every other variable
  present in the netCDF output beyond the declared `vars_sim` set – see
  [`?read_glm_output`](https://limnotrack.com/reference/read_glm_output.md).
  Ignored for other models. Defaults to TRUE.

- use_dat:

  logical; for the Simstrat models only, read Simstrat's own
  `<var>_out.dat` text output via
  [`read_simstrat_dat`](https://limnotrack.com/reference/read_simstrat_dat.md)
  instead of the consolidated `output.nc`. Every other argument means
  the same thing either way, so this only changes where the numbers are
  read from. `TRUE` is the faster path – it skips the netCDF entirely,
  and with `load_all = FALSE` reads only the files the requested
  `vars_sim` need, which is what a calibration wants. Defaults to
  `NULL`: read `output.nc` when there is one, and fall back to the text
  output when there is not (a run whose output was never converted, or
  converted with
  [`write_simstrat_nc`](https://limnotrack.com/reference/write_simstrat_nc.md)`(remove_dat = FALSE)`
  and the netCDF since removed). Ignored when `nc` is supplied.

- daily_mean:

  logical; when `TRUE`, return one record per calendar day. If a model's
  daily-mean `output_daily.nc` companion is present (GOTM writes one
  natively; GLM-AED and Simstrat get one from
  [`run_aeme()`](https://limnotrack.com/reference/run_aeme.md) when
  `time(aeme)$output_daily_mean` is `TRUE`) it is read directly;
  otherwise the raw sub-daily `output.nc` is read and averaged by
  calendar day. Defaults to `FALSE`. See
  [`set_output_time_step`](https://limnotrack.com/reference/set_output_time_step.md).

## Value

List of model outputs in AEME standard format
