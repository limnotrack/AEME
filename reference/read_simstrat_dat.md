# Read Simstrat's raw text output directly

Reads Simstrat's own `<var>_out.dat` text files straight from the
simulation's output directory, returning the same output list as
[`read_simstrat_output`](https://limnotrack.com/reference/read_simstrat_output.md)
does from the consolidated `output.nc` – same keys, same depth grid,
same unit conversions – without going through netCDF at all.

## Usage

``` r
read_simstrat_dat(
  sim_folder = NULL,
  vars_sim = NULL,
  depths = NULL,
  dates = NULL,
  date_index = NULL,
  incl_fluxes = TRUE,
  load_all = FALSE,
  raw_output = FALSE,
  model = "simstrat_aed2",
  config_file = "simstrat.par",
  out_dir = NULL,
  ref_year = NULL
)
```

## Arguments

- sim_folder:

  character; path to the `simstrat_aed2`/`simstrat_aed` simulation
  directory (containing `simstrat.par` and the output directory it
  points at). Not needed if both `out_dir` and `ref_year` are supplied.

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

- load_all:

  logical; also load every other variable present in the output
  directory beyond the declared `vars_sim` set. Default `FALSE` – the
  opposite of
  [`read_simstrat_output`](https://limnotrack.com/reference/read_simstrat_output.md)'s
  default, because loading every variable means reading every file,
  which is the cost this function exists to avoid.

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

- model:

  character; which Simstrat coupling this output came from,
  `"simstrat_aed2"` (default) or `"simstrat_aed"`. Selects the matching
  `key_naming` column for variable-name translation – the netCDF file
  format itself (produced by
  [`write_simstrat_nc`](https://limnotrack.com/reference/write_simstrat_nc.md))
  is identical either way.

- config_file:

  character; name of (or path to) the Simstrat configuration file.
  Default `"simstrat.par"`.

- out_dir:

  character; the output directory itself, skipping the `simstrat.par`
  lookup. Default `NULL` (read from `simstrat.par`).

- ref_year:

  integer; the simulation's `Simulation.Reference year`, skipping the
  `simstrat.par` lookup. Default `NULL` (read from `simstrat.par`).
  Supply both this and `out_dir` in a tight loop to avoid re-parsing the
  configuration on every read.

## Value

List with AEME output variables, classed as for
[`read_simstrat_output`](https://limnotrack.com/reference/read_simstrat_output.md),
or a `model_output_error` (see
[`is_model_error`](https://limnotrack.com/reference/is_model_error.md))
if the output directory holds no readable output.

## Details

This is the fast path for workflows that read model output once per
model run, such as `aemetools`' calibration and sensitivity analyses.
Reading the text directly avoids both the netCDF write (which serialises
*every* output variable, compressed) and the netCDF read, and only the
files actually asked for are parsed: with `load_all = FALSE` (the
default here, unlike
[`read_simstrat_output`](https://limnotrack.com/reference/read_simstrat_output.md))
a single-variable read touches one `<var>_out.dat` file plus the small
`WaterH_out.dat`, however many variables the run wrote. When
`date_index`/`dates` select a window of the simulation, only rows up to
the end of that window are parsed.

Simstrat writes every water-column variable on the same output depth
grid (`Output.Depths` in `simstrat.par`), so – exactly as in the netCDF
the `.dat` files are otherwise converted to – depth profiles share one
`z` grid, taken from the first depth-varying file. AED sediment-zone
output (`<var>_zone_out.dat`) is on its own zone axis instead, and is
returned as an
[`new_grouped_var`](https://limnotrack.com/reference/new_grouped_var.md)
object, again matching
[`read_simstrat_output`](https://limnotrack.com/reference/read_simstrat_output.md).

Note that this needs the `.dat` files to still be there: they are kept
by default, but
[`write_simstrat_nc`](https://limnotrack.com/reference/write_simstrat_nc.md)`(remove_dat = TRUE)`
deletes them once they have been written to `output.nc`.

## See also

[`read_simstrat_output`](https://limnotrack.com/reference/read_simstrat_output.md)
to read the same output back from `output.nc`,
[`write_simstrat_nc`](https://limnotrack.com/reference/write_simstrat_nc.md)
to produce that file, and
[`read_simstrat_dat_file`](https://limnotrack.com/reference/read_simstrat_dat_file.md)
for the single-file primitive.
