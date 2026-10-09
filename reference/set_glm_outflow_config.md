# Fine-grained control of the GLM-AED `&outflow` configuration

Rewrites the `&outflow` block of a GLM nml file with per-outlet
withdrawal settings and block-level options, validated against GLM's own
rules (see `src/glm_init.c` in the GLM source).
[`set_glm_outflows()`](https://limnotrack.com/reference/set_glm_outflows.md)
is the thin writer
[`build_aeme()`](https://limnotrack.com/reference/build_aeme.md) uses
and only distinguishes fixed vs floating outlets; this function
additionally exposes adaptive (temperature-targeting) outlets, submerged
(type 6) outlets, per-outlet critical-withdrawal thresholds,
target-withdrawal-temperature forcing, bed seepage and weir geometry.

## Usage

``` r
set_glm_outflow_config(
  path_glm,
  outlets = NULL,
  surface_elev = NULL,
  seepage = NULL,
  seepage_rate = NULL,
  withdr_temp_file = NULL,
  adaptive = NULL,
  crest_width = NULL,
  crest_factor = NULL,
  thick_limit = NULL,
  single_layer_draw = NULL,
  extra = NULL,
  bathy = NULL,
  dims_lake = NULL,
  validate = TRUE,
  glm_file = find_glm_nml(path_glm, must_exist = FALSE)
)
```

## Arguments

- path_glm:

  GLM-AED directory. Used to locate the nml when `glm_file` is not given
  and to resolve relative `file` paths for existence checks.

- outlets:

  data.frame, one row per outlet. Recognised columns (all optional
  except where noted):

  `name`

  :   outlet label, used only in messages.

  `type`

  :   GLM `outlet_type`: `1` fixed, `2` floating, `3`
      adaptive/target-temperature, `4`-`5` other withdrawal modes, `6`
      submerged. Default `2`.

  `float`

  :   logical `flt_off_sw`. Default `type == 2`. GLM forces a floating
      outlet to `type = 2`.

  `elev`

  :   outlet elevation (m, hypsography datum) - see *Outlet elevation
      convention*. Required for every outlet unless the existing block
      already has an `outl_elvs` entry for it.

  `bsn_len`, `bsn_wid`

  :   basin length / width at the outlet (m). Computed from the
      hypsography when omitted.

  `factor`

  :   `outflow_factor`, per-outlet flow multiplier. Default `1`.

  `file`

  :   `outflow_fl`, path to the outlet's flow-forcing CSV, relative to
      `path_glm`. Carried over from the existing block when omitted and
      the outlet count is unchanged.

  `target_temp`

  :   `target_temp` (degC) for `type == 3`.

  `crit`

  :   `outlet_crit`, per-outlet critical threshold (`Hcrit`).

  `subm_elev`

  :   `subm_elev_outflow` for `type == 6`: a fixed submerged elevation
      as height above the bed (m), `0 <= subm_elev <= max depth`.

  `elev_idx`

  :   `elev_idx_outflow` for `type == 6`: a dynamic layer index that
      overrides `subm_elev`. Use `NA` / `-1` for none.

- surface_elev:

  current lake surface elevation (m, hypsography datum), used to convert
  a floating outlet's `elev` to a depth below the surface. Default:
  `min(H) + lake_depth` read from the nml.

- seepage, seepage_rate:

  enable constant bed seepage and set its rate (m/day). `NULL` leaves
  the current value.

- withdr_temp_file:

  `withdrTemp_fl`: a single target-withdrawal- temperature forcing CSV
  (relative to `path_glm`) shared by the adaptive outlets. `NULL` leaves
  the current value; `NA` removes it.

- adaptive:

  named list of block-level adaptive-withdrawal controls, any of
  `crit_val`, `crit_dep`, `crit_days`, `crit_above`, `crit_varname`,
  `crit_idx`, `min_lake_temp`, `fac_range_upper`, `fac_range_lower`,
  `mix_withdraw`, `coupl_oxy_sw`. Only supplied names are written.

- crest_width, crest_factor:

  weir / overflow geometry for the surface outlet. `NULL` leaves the
  current value.

- thick_limit:

  `outflow_thick_limit`: minimum layer thickness (m) an outlet will draw
  from. `NULL` leaves the current value.

- single_layer_draw:

  logical `single_layer_draw`: force each outlet to draw from the single
  layer at its elevation. `NULL` leaves the current value.

- extra:

  named list of any further raw `&outflow` keys to set verbatim (e.g.
  `time_fmt`, `timezone`). Applied last, so it overrides the above.

- bathy:

  data.frame with `elev` / `area` columns (the hypsograph), used to size
  outlets and to validate elevations. Default: the `H` / `A` arrays in
  the nml's `&morphometry` block.

- dims_lake:

  length-2 numeric `c(basin_length, basin_width)` at the crest. Default:
  the nml's `bsn_len` / `bsn_wid`.

- validate:

  check every value against GLM's ranges before writing and abort on any
  violation. Default `TRUE`.

- glm_file:

  path to the GLM nml. Default: discovered under `path_glm`.

## Value

invisibly, the updated nml list (also written to `glm_file`).

## Details

Only the nml is updated - no outflow forcing CSVs are created or
modified. Point each outlet's `file` at a CSV you have already written
(for example with
[`set_glm_outflows()`](https://limnotrack.com/reference/set_glm_outflows.md)
or [`build_aeme()`](https://limnotrack.com/reference/build_aeme.md)).
Keys already present in the `&outflow` block that you do not set here
are left untouched.

## Outlet elevation convention

`elev` is always given as an absolute elevation on the hypsography /
`&morphometry` `H` datum. For a fixed outlet it is written straight to
`outl_elvs` (GLM requires `base_elev <= elev <= crest_elev`). For a
floating offtake GLM instead wants a depth *below the moving surface*,
so `elev` is converted with `surface_elev - elev` and must satisfy
`0 <= surface_elev - elev <= (crest_elev - base_elev)`.

## See also

[`set_glm_outflows()`](https://limnotrack.com/reference/set_glm_outflows.md),
[`build_aeme()`](https://limnotrack.com/reference/build_aeme.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# A floating offtake 2 m below the surface plus a fixed bottom gate,
# with bed seepage switched on.
set_glm_outflow_config(
  path_glm,
  outlets = data.frame(
    name   = c("spillway", "bottom_gate"),
    type   = c(2L, 1L),
    elev   = c(surface_elev - 2, base_elev + 0.5),
    file   = c("bcs/outflow_spillway.csv", "bcs/outflow_bottom_gate.csv")
  ),
  seepage = TRUE, seepage_rate = 0.001
)

# An adaptive outlet targeting 12 degC.
set_glm_outflow_config(
  path_glm,
  outlets = data.frame(type = 3L, elev = surface_elev - 5,
                       target_temp = 12,
                       file = "bcs/outflow_wbal.csv"),
  adaptive = list(min_lake_temp = 4, fac_range_upper = 1.2,
                  fac_range_lower = 0.8)
)
} # }
```
