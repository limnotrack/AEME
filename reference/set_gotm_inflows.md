# Set inflow data for a GOTM-WET simulation directory

Thin, `aeme`-free wrapper around the internal inflow writer used by
[`build_aeme()`](https://limnotrack.com/reference/build_aeme.md). Writes
`inputs/inf_{flow,temp,salt}_<name>.dat` (and, if BGC is coupled,
`inputs/inf_chem_<name>.dat`) per inflow into `path_gotm`, and updates
the `streams` block of `gotm.yaml` to point at them. Existing stream
entries not named in `inf_list` are left untouched.

## Usage

``` r
set_gotm_inflows(
  path_gotm,
  inf_list,
  inf_factor = 1,
  use_bgc = NULL,
  yaml_file = file.path(path_gotm, "gotm.yaml")
)
```

## Arguments

- path_gotm:

  filepath; to GOTM-WET directory (containing `gotm.yaml` and an
  `inputs/` subdirectory)

- inf_list:

  named list of data.frames, one per inflow. Each must have a `Date`
  column plus flow/temperature/salt columns (e.g. `HYD_flow`,
  `HYD_temp`, `CHM_salt`) – see
  [`add_inflow()`](https://limnotrack.com/reference/add_inflow.md) for
  the expected schema.

- inf_factor:

  numeric; scaling factor applied to all inflow flow rates. Default is
  `1`.

- use_bgc:

  logical; also write BGC concentration inflow files. Defaults to the
  existing `fabm.use` setting in `gotm.yaml`.

- yaml_file:

  filepath; path to the `gotm.yaml` file to update. Defaults to
  `gotm.yaml` in `path_gotm`.

## Value

invisibly, the updated yaml list

## Examples

``` r
if (FALSE) { # \dontrun{
set_gotm_inflows(path_gotm, inf_list = list(stream1 = inflow_df))
} # }
```
