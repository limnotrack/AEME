# Set inflow data for a DYRESM-CAEDYM simulation directory

Thin, `aeme`-free wrapper around the internal inflow writer used by
[`build_aeme()`](https://limnotrack.com/reference/build_aeme.md). Writes
`<lakename>.inf` into `path_dy` and, so the inflow set stays consistent,
rebuilds the inflow block of `<lakename>.stg` (see `.rewrite_dy_stg()`
note about inflow geometry defaults).

## Usage

``` r
set_dy_cd_inflows(path_dy, list_inf, inf_factor = 1, update_stg = TRUE)
```

## Arguments

- path_dy:

  filepath; the `dy_cd` model directory (containing the `<lakename>.stg`
  file).

- list_inf:

  named list of data.frames, one per inflow. Each must have a `Date`
  column plus flow/temperature/salt columns (e.g. `HYD_flow`,
  `HYD_temp`, `CHM_salt`) – see
  [`add_inflow()`](https://limnotrack.com/reference/add_inflow.md) for
  the expected schema. Column names are translated to DYRESM-CAEDYM's
  own via `rename_modelvars()`.

- inf_factor:

  numeric; scaling factor applied to all inflow flow rates. Default is
  `1`.

- update_stg:

  logical; also rebuild the inflow block of `<lakename>.stg` to match
  `names(list_inf)`. Default `TRUE`.

## Value

invisibly, `NULL`.

## Examples

``` r
if (FALSE) { # \dontrun{
set_dy_cd_inflows(path_dy, list_inf = list(FWMT = inflow_df))
} # }
```
