# Set outflow data for a Simstrat-AED2 simulation directory

Thin, `aeme`-free wrapper around the internal outflow writer used by
[`build_aeme()`](https://limnotrack.com/reference/build_aeme.md). Writes
a combined `Qout.dat` into `path_simstrat`. All outflows are summed into
a single series at a single representative withdrawal elevation,
matching `make_wdr_simstrat()`'s existing behaviour.

## Usage

``` r
set_simstrat_outflows(
  path_simstrat,
  outf,
  heights_wdr,
  surface_elev,
  outf_factor = 1,
  ref_year = NULL,
  par_file = file.path(path_simstrat, "simstrat.par")
)
```

## Arguments

- path_simstrat:

  filepath; to Simstrat-AED2 directory

- outf:

  named list of data.frames, one per outflow, each with a `Date` column
  and a `HYD_flow` column – see
  [`add_outflows()`](https://limnotrack.com/reference/add_outflows.md)
  for the expected schema.

- heights_wdr:

  named numeric vector; withdrawal elevation (m, absolute – the same
  datum as `hyps$elev`/`surface_elev`) for each name in `outf`.

- surface_elev:

  numeric; the lake surface elevation (m) that this Simstrat-AED2
  configuration was built with – the zero-point for
  `Bathymetry.dat`/`InitialConditions.dat`/inflow-outflow depths (see
  `make_stg_simstrat()`). Unlike GLM's nml, Simstrat's own files only
  store depths already relative to this elevation, so it cannot be
  recovered from `path_simstrat` alone and must be supplied.

- outf_factor:

  numeric; scaling factor applied to all outflow flow rates. Default is
  `1`.

- ref_year:

  integer; Simstrat's `Simulation.Reference year`. Defaults to the value
  already in `simstrat.par`.

- par_file:

  filepath; path to the `simstrat.par` file to read `use_bgc`/`ref_year`
  defaults from. Defaults to `simstrat.par` in `path_simstrat`.

## Value

invisibly, `NULL`

## Examples

``` r
if (FALSE) { # \dontrun{
set_simstrat_outflows(path_simstrat, outf = list(outlet_1 = outflow_df),
                      heights_wdr = c(outlet_1 = 10.5), surface_elev = 13.07)
} # }
```
