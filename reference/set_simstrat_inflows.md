# Set inflow data for a Simstrat-AED2 simulation directory

Thin, `aeme`-free wrapper around the internal inflow writer used by
[`build_aeme()`](https://limnotrack.com/reference/build_aeme.md). Writes
`Qinp.dat`/`Tinp.dat`/`Sinp.dat` (and, if BGC is coupled,
`AED2_inflow/*.dat`) into `path_simstrat`.

## Usage

``` r
set_simstrat_inflows(
  path_simstrat,
  inf,
  inf_factor = 1,
  use_bgc = NULL,
  model_controls = NULL,
  ref_year = NULL,
  par_file = file.path(path_simstrat, "simstrat.par")
)
```

## Arguments

- path_simstrat:

  filepath; to Simstrat-AED2 directory

- inf:

  named list of data.frames, one per inflow. Each must have a `Date`
  column plus `HYD_flow`, `HYD_temp`, `CHM_salt` columns – see
  [`add_inflow()`](https://limnotrack.com/reference/add_inflow.md) for
  the expected schema. Multiple inflows are merged into a single series
  (Simstrat only accepts one): flow is summed and the scalars
  (temperature, salinity, BGC) are flow-weighted so total load is
  conserved – see `make_inf_simstrat()`.

- inf_factor:

  numeric; scaling factor applied to all inflow flow rates. Default is
  `1`.

- use_bgc:

  logical; also write AED2 inflow concentration files. Defaults to the
  existing `ModelConfig.CoupleAED2` setting in `simstrat.par`.

- model_controls:

  data.frame of loaded model controls (see
  [`get_model_controls()`](https://limnotrack.com/reference/get_model_controls.md)),
  required when `use_bgc = TRUE`.

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
set_simstrat_inflows(path_simstrat, inf = list(stream1 = inflow_df))
} # }
```
