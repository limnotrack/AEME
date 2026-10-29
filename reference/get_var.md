# Get variable from aeme

Get variable from aeme

## Usage

``` r
get_var(
  aeme,
  model,
  var_sim,
  depth = NULL,
  depth_ref = c("surface", "bottom"),
  return_df = TRUE,
  ens_n = 1,
  use_obs = FALSE,
  remove_spin_up = TRUE,
  cumulative = FALSE,
  zone = NULL,
  sed_layer = NULL
)
```

## Arguments

- aeme:

  Aeme object, or an
  [AemeSummary](https://limnotrack.com/AEME/reference/AemeSummary.md).
  For a summary only `model` and `var_sim` apply, and the result has the
  surface and bottom series (`layer`) and every `stat`; filter
  [`output()`](https://limnotrack.com/AEME/reference/output.md) for more
  control.

- model:

  character vector; models to use. One or more of `"dy_cd"`,
  `"glm_aed"`, `"gotm_wet"`, `"simstrat_aed2"`. Defaults to all models
  if not found in `aeme`.

- var_sim:

  character; variable in the AEME format (e.g. "HYD_temp").

- depth:

  numeric; depth of the variable to extract, referenced to `depth_ref`.
  Default is NULL. If NULL, the variable profiles are extracted.

- depth_ref:

  character; what `depth` is measured from when a single depth is
  extracted. `"surface"` (default): metres below the water surface.
  `"bottom"`: metres above the lake bed, so `depth = 1` is the water 1 m
  above the bottom. That is `level - depth` metres below the surface,
  with `level` the modelled water level (`LKE_lvlwtr`) on each date, so
  it follows the surface as the level rises and falls. On a date when
  the level is below `depth` (the point would be above the surface) the
  value is `NA`. Ignored when `depth` is `NULL`. (In AEME 0.4.0 and
  earlier `"bottom"` was measured from the basin's full depth, which
  only matches when the level equals that depth.)

- return_df:

  logical; if TRUE, return a dataframe; if FALSE, return a list. Default
  is TRUE.

- ens_n:

  integer; ensemble number to plot. Default is 1.

- use_obs:

  logical; if TRUE, use observations to extract the variable at time and
  depth of observations. Default is FALSE. Use this option if you want
  to compare model output to observations. Observations recorded as a
  height above the lake bed (see
  [`add_obs()`](https://limnotrack.com/AEME/reference/add_obs.md)) are
  compared with the model profile at (the model's water level - that
  height) on each date, so they follow the model's own level rather than
  the depth they were placed at when added.

- remove_spin_up:

  logical; remove spin-up period from plot. Default is TRUE.

- cumulative:

  logical; if TRUE, return cumulative sum of variable

- zone:

  integer; for GLM-AED sediment-zone variables (those with an `nzones`
  column, e.g. `"SED_Fsed_oxy"`, `"SED_temp"`), keep only these sediment
  zones (GLM zone order, 1 = deepest). `NULL` (default) keeps all.

- sed_layer:

  integer; for variables with a sediment-layer dimension (`"SED_temp"`),
  keep only these layers (1 = shallowest). `NULL` (default) keeps all.
  Sediment variables are returned in long format with one column per
  dimension (`nzones`, `sed_layers`); `depth_ref`/`depth` do not apply.

## Value

If `return_df = TRUE`, a data frame with the same leading columns
whether or not `use_obs` is used: `Date`, `depth` (`NA` for variables
without a depth dimension), `value`, `var_sim` and `Model`. With
`use_obs = TRUE` the column `obs` (the observation, paired with the
model `value`) is added after `value`, plus `depth_ref` and
`height_above_bed` when height-above-bed observations are present.
Variables with non-standard dimensions (e.g. sediment zones) carry one
column per extra dimension instead of `depth`. If `return_df = FALSE`, a
list with one such data frame per model.
