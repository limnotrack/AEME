# GLM sediment-temperature parameters from observed temperature profiles

Fits an annual sediment-temperature cycle (`sed_temp_mean`,
`sed_temp_amplitude`, `sed_temp_peak_doy`) for each GLM sediment zone
from observed water-column temperature profiles. Used internally by
[`build_aeme()`](https://limnotrack.com/reference/build_aeme.md) to
populate the GLM `&sediment` block, and exported so the same per-zone
values can be generated as a calibration parameter table.

## Usage

``` r
calc_sed_temp(
  aeme = NULL,
  obs_temp = NULL,
  hypsograph = NULL,
  sed_zones = NULL,
  max_depth = NULL,
  temp_var = "HYD_temp",
  nml_file = "glm4.nml",
  output = c("parameters", "nml", "summary"),
  depth_grid = 0.25,
  depth_tol = 2,
  min_obs = 10,
  min_months = 6,
  default_mean = 12,
  default_amplitude = 4,
  default_peak_doy = 46,
  borrow_amp_factor = 0.6,
  borrow_lag_days = 25,
  verbose = TRUE
)
```

## Arguments

- aeme:

  `Aeme` object; when supplied, `obs_temp` and `hypsograph` are
  extracted from it unless passed explicitly. Default `NULL`.

- obs_temp:

  long df: Date, var_aeme, depth, value (degC), depths in m below
  surface. Optional when `aeme` is given.

- hypsograph:

  df with `depth` (m, +down) or `elev`, and `area` (m2), used to
  area-weight within a zone and (with `sed_zones = NULL`) to derive the
  zones. Optional when `aeme` is given. `NULL` -\> uniform weighting and
  `sed_zones` / `max_depth` must be supplied.

- sed_zones:

  numeric vector of zone upper-boundary heights above the bed,
  ascending - exactly as passed to GLM `zone_heights`. Zone 1 is 0 -\>
  sed_zones\[1\] (the deepest slab); the last value may exceed max
  depth. `NULL` (default) -\>
  [`estimate_sed_zones()`](https://limnotrack.com/reference/estimate_sed_zones.md)
  on `hypsograph`.

- max_depth:

  max lake depth (m). NULL -\> from hypsograph, else deepest obs
  (warns).

- temp_var:

  value of var_aeme to keep.

- nml_file:

  character; name of the GLM nml file the parameters belong to, used for
  the `file` column of the parameter table. Default `"glm4.nml"`.

- output:

  one of `"parameters"` (default) - a long AEME model-parameter table in
  [`param_colnames()`](https://limnotrack.com/reference/param_colnames.md)`(incl_opt = FALSE)`
  order, ready for calibration; `"nml"` - a named list of the three
  per-zone vectors to splice into `glm_nml[["sediment"]]`; or
  `"summary"` - the per-zone diagnostic data.frame with `parameters` /
  `nml` / `zone_series` attributes.

- depth_grid:

  within-zone integration step (m).

- depth_tol:

  a cast is used for a zone only if its deepest sample is within
  depth_tol m of the top of that zone's depth band.

- min_obs, min_months:

  thresholds for a harmonic fit (else monthly-range).

- default_mean, default_amplitude, default_peak_doy:

  last-resort values. default_peak_doy = 46 is mid-Feb (S. hemisphere
  summer); use ~209 for N.

- borrow_amp_factor, borrow_lag_days:

  when an unfitted deep zone borrows a cycle from the nearest shallower
  fitted zone: damp amplitude by borrow_amp_factor^(zone steps) and lag
  peak_doy by borrow_lag_days per step.

- verbose:

  logical; print the assembled per-zone table, nml snippet and parameter
  table.

## Value

Depends on `output`:

- `"parameters"` - data.frame with columns `model`, `file`, `name`,
  `value`, `min`, `max`, `group`, `index`; one row per zone for each of
  `sediment/sed_temp_mean`, `sediment/sed_temp_amplitude` and
  `sediment/sed_temp_peak_doy`.

- `"nml"` - named list `sed_temp_mean` / `sed_temp_amplitude` /
  `sed_temp_peak_doy`, each a per-zone numeric vector in GLM zone order.

- `"summary"` - the per-zone diagnostic data.frame (zone 1 = deepest),
  carrying the `"parameters"` table, a ready-to-paste `"nml"` snippet
  and the per-zone `"zone_series"` as attributes.

## Details

Inputs can be supplied directly (`obs_temp`, `hypsograph`, `sed_zones`)
or pulled from an [Aeme](https://limnotrack.com/reference/Aeme.md)
object via `aeme`: observations come from
[`get_obs()`](https://limnotrack.com/reference/get_obs.md) and the
hypsograph from [`input()`](https://limnotrack.com/reference/input.md).
`sed_zones` defaults to
[`estimate_sed_zones()`](https://limnotrack.com/reference/estimate_sed_zones.md)
on the hypsograph when not given.
