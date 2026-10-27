# AEME 0.4.0.9000 (development version)

## Inflow silica units

* `SIL_rsi` inflow values are no longer auto-converted. Previously a median
  > 100 was assumed to be mg/L and multiplied by 1000/60.084, so valid
  mmol/m3 inputs of 150-300 were inflated ~16.6x. Silica must now be supplied
  in mmol/m3.

## GLM-AED initial water-quality values

* `&init_profiles` `wq_init_vals` are now converted from AEME units to GLM-AED
  native units by *dividing* by `conversion_aed` (as for the AED nml, inflows
  and Simstrat). They were multiplied, so e.g. DOC of 0.5 g/m3 was written as
  0.006 instead of 41.63 mmol/m3. Rebuild existing GLM-AED models to pick up
  the fix.
* `set_glm_init(wq_init = )` now takes AEME (standard) units, e.g.
  `CHM_oxy = 10` mg/L, keyed by AEME variable name, and converts to GLM-AED
  native units (mmol/m3), rounded to 4 decimal places (as are the values
  written at build). Names that only match GLM `wq_names` are written
  as-is.

## Calibrate inflow variables

* Parameters with `file = "inf"` can now name an inflow variable (e.g.
  `CAR_doc`) to multiply it, and the new `file = "inf_const"` sets it to a
  constant. Previously `inf` only scaled flow (name `"inflow"`, unchanged),
  so concentrations could not be varied independently. `inflow_param()`
  builds the rows.

## `get_var()` returns consistent columns

* `get_var()` now returns `Date`, `depth`, `value`, `var_sim` and `Model` with
  and without `use_obs`; `use_obs = TRUE` adds `obs` after `value`. The model value is now `value`
  (previously `sim`), `var_aeme` is no longer returned (use `var_sim`), and
  level/scalar variables always have a `depth` column. The
  function was split into internal helpers.

## `plot_aed_succession()`

* New `plot_aed_succession()` plots a chlorophyll a time series above a monthly
  composite (0-100 %) of each phytoplankton group's contribution.

## `view_aed_modules()`

* New `view_aed_modules()` opens an interactive diagram of the AED module
  interactions in the viewer pane or web browser.
* The diagram is also embedded in the GLM-AED article via a self-contained
  iframe (`srcdoc`), so its styling does not affect the article.

## Fixes

* `run_aeme(ens_n = )` with a number other than 1 now adds (or replaces) that
  ensemble member instead of discarding the existing output; `ens_n = 1`
  still starts from empty output.
* `check_glm_nml()` no longer errors when the GLM `&outflow` block has no
  `outflow_fl` (all spill through GLM's weir), and `build_aeme()` no longer
  errors in `calc_lake_obs_deriv()` when there are no lake observations.
* `set_glm_outflow_config()` warns correctly when several outflow files are
  missing.

## `aeme_to_inflow()` uses the GLM-AED mass balance

* For `glm_aed`, `aeme_to_inflow()` now takes the concentrations from
  `mass_balance.csv` when it exists: the mass leaving the lake (`Out`) divided
  by the outflow volume, i.e. the flow-weighted concentration of the water that
  actually leaves, converted to AEME units. This replaces the concentration at
  0.25 m depth for the variables in the mass balance (at most 15, see
  `set_glm_balance_groups()`), adds `NIT_tn` and `PHS_tp` from the summed totals,
  and falls back to the 0.25 m value on days without outflow or for variables
  not in the file.

## Read and diagnose the GLM-AED mass balance

* New `read_glm_mass_balance()` reads `mass_balance.csv` (AED native units,
  mmol) into a long table. `calc_aed_totals_balance()` sums the components into
  the `aed_totals` TN, TP, TOC (and TSS), optionally converting to kg, and
  `diag_glm_mass_balance()` returns the daily and summary
  `d(Lake) - (Inf - Out)` remainder, per total or per variable (all accept an
  `aeme` object to locate the file), with an
  optional cumulative plot. `Inf` and `Out` are totals over the save interval
  (`nsave * dt`), so the diagnostics use the interval between rows and also
  return per-day rates, and warn if the spacing disagrees with the GLM nml.

## GLM-AED mass balance reports the components of the totals

* The GLMv4 `&mass_balance` `balance_vars` are now the components summed into
  the `aed_totals`, derived with the same per-phytoplankton-group logic as
  `set_aed_totals()` (internal-quota pools versus biomass x fixed
  stoichiometry), so the totals can be summed from `mass_balance.csv`.
  GLM allows at most 15 `balance_vars`, so by default only the nitrogen (TN),
  phosphorus (TP) and suspended solids (TSS) groups are reported. New
  `set_glm_balance_groups()` chooses the groups (`"TN"`, `"TP"`, `"TOC"`,
  `"TSS"`, `"other"`) for an existing configuration, and the default for new
  builds is set with `options(AEME.glm_balance_groups = )`. Components whose
  AED module is not active are skipped, and lists over 15 are truncated with a
  warning.

## Derived lake observations no longer accumulate

* `build_aeme()` appended the derived oxygen observations (`CHM_oxycln`,
  `CHM_oxynal`) to `observations(aeme)$lake` again on every build of an
  object that had already been built, so the table grew each time. Derived
  records already present are now skipped. `CHM_oxyhyp` was also never written
  because of a duplicated list name; it is now added.

## `build_aeme()` reuses the stored water balance

* The water balance, the slowest estimate in `build_aeme()`, is now hashed
  from its inputs (meteorology, inflows, outflows, lake level, hypsograph,
  times, models, `wb_method`, `coeffs`, observed water temperature and the AEME
  version) and the hash is stored in `water_balance(aeme)$hash`. Rebuilding
  with unchanged inputs, for example after changing only parameters or model
  controls, reuses the stored result. New `refresh_wbal = TRUE` argument forces
  a recalculation.

## GLM-AED spills through its own weir by default

* New `glm_weir` argument to `build_aeme()` (default `TRUE`, also read from
  `configuration()`). GLM-AED no longer reads the water-balance (`wbal`) outflow
  file; it spills through its own weir (`morphometry/crest_elev`,
  `outflow/crest_width`, `outflow/crest_factor`). The weir is fitted to the
  observed `"outflow"` and lake level when at least 10 days overlap (that
  outflow is then replaced by the weir); otherwise it is taken from the
  water-balance `C` and `h_inv` (`wb_method` 2 or 3). Other outflows stay as
  GLM outlets. When fitted from observations, the `glm_aed` water-balance `C` /
  `h_inv` (shared with `dy_cd`) are set to the equivalent values, so the water
  balance no longer runs `optim()` for that family and matches GLM. Use
  `glm_weir = FALSE` for the previous behaviour; `glm_config_to_aeme()` objects
  default to `FALSE`.

## GLM-AED southern hemisphere albedo workaround

* GLM-AED currently treats the seasonal albedo cycle of southern hemisphere
  lakes as if they were in the northern hemisphere. As a temporary catch,
  `build_glm()` now writes `albedo_mean = 0.08` and `albedo_amplitude = 0.02`
  (or `-0.02` when the latitude is negative) to the `&light` block of the GLM
  nml. **Caveat:** this is a stop-gap and should be removed once the bug is
  fixed in GLM-AED.

## Clearer observations in `plot_ts()`

* Observations are now drawn as black points with a white outline (or, when several variables are plotted, points filled to match each line with a black outline) so they stand out from the model lines, and the new `obs_size` argument controls their size (default 2.5).

## Trophic level index calculated from annual averages

* The derived trophic level indices (`LKE_tlic`, `LKE_tlin`, `LKE_tlip`,
  `LKE_tlise`, `LKE_tli3`, `LKE_tli4`) are now calculated from annual averages
  of chlorophyll-a, total nitrogen, total phosphorus and Secchi depth, as in
  the NZ lake trophic level protocol, which deseasonalises the data. The
  epilimnetic mean is averaged over each hydrological year (July to June; set
  the first month with `options(AEME.tli_year_start_month = )`) before the
  index equations are applied, and every time step in a year carries that
  year's value. Observed `LKE_tli3`/`LKE_tli4` use the same annual averaging
  and need at least four sampling dates in a year.
* **Behaviour change.** The Secchi component now uses the protocol equation
  (`5.10 + 2.27 * log10(1/SD - 1/40)`), previously `5.56 + 2.6 * ...`, so
  `LKE_tlise` and `LKE_tli4` differ from earlier versions.
* Fixed the dependency of `LKE_tlin` in `key_naming` (`NIT_tn`, not `PHS_tn`).

## `AemeSummary`: a lightweight summary for dashboards

* New S4 class `AemeSummary`, made by `summarise_aeme()` (now exported, and also
  what `summary(<Aeme>)` returns). It holds daily surface and bottom series for
  every depth-resolved variable (split at `HYD_epidep`, else `HYD_thmcln`, else
  half the water column), variables without depth, the observations, daily
  meteo/inflow/outflow/water-balance forcing, the hypsograph, the parameters and
  a `meta` list. There is no model configuration, so it cannot be built or run.
  With several ensemble members, `stats` (mean, min, max, quantiles) summarise
  them, and `keep_members = TRUE` keeps each member too. `profiles = TRUE` adds
  seasonal mean profiles.
* The `output` slot is a long table (`Date`, `model`, `var_aeme`, `layer`,
  `stat`, `value`). Accessors `lake()`, `time()`, `observations()`, `output()`
  and `parameters()` work on it, with new `forcing()`, `profiles()`, `models()`
  and `hypsograph()`. `get_var()` and `plot()` accept it.
* `write_aeme_summary()` / `read_aeme_summary()` store it as `rds`, `parquet`
  or (write only) `json`.
* **Behaviour change.** The old `summarise_aeme()` replaced an `Aeme`'s output
  with 2-column surface/bottom matrices, recognised by `check_if_summary()`.
  That is gone (as are `get_var_summary()` and `plot_var_summary()`): an `Aeme`
  always holds full output.

## Consistent structure for the `output` slot

* The layout of `output()` (`n_members` plus one `ens_NNN` list per member, each
  named by model) is now built and read through shared internal helpers instead
  of repeated `grepl("ens", ...)` and hand-written model lists. `n_members` is
  always counted from the members present, and members are matched by exact
  name (`^ens_[0-9]+$`).
* `Aeme` validity now checks `output`: `n_members` must be a single number and
  each member a list of `NULL`/lists named by a known model.
* `remove_output()` and a new `Aeme` object share one empty-output form
  (`list(n_members = 0L)`); `run_aeme_ensemble()` and `read_aeme_from_files()`
  store every model key (`NULL` when absent), as `load_output()` already did.
* The `output` slot documentation now describes the member structure and
  includes `simstrat_aed`.

## Choosing which GLM-AED sediment, flux and diagnostic output to keep

GLM-AED writes far more than the standard variables: sediment temperature and
heat flux, sediment-water fluxes (lake-wide and by sediment zone), sediment
pools and AED diagnostics. This version makes them easy to request, name and
use, e.g. from `aemetools` sensitivity analysis and calibration, without
carrying the whole payload in every ensemble member.

* **Behaviour change.** `run_aeme()`, `run_aeme_ensemble()` and `load_output()`
  now read only the standard variables (`vars_sim` and the lake fluxes) by
  default; the extra GLM-AED output has to be requested with `extra_vars`.
  Previously every variable in `output.nc` was loaded and stored on every run.
  Loading after a run is about four times faster by default (0.6 s against 2.4 s
  in a test lake) and the stored output is 3.3 MB instead of 18 MB. Use
  `extra_vars = "all"` for the old behaviour. `read_glm_output()` and
  `read_model_outputs()`, which read a file you point them at, still read
  everything unless told otherwise.

* `load_output()` gains `return_type`. `"output"` returns the output as a list
  of `aeme_output` objects, one per model, without storing it on the `Aeme`
  object, so extra output can be explored without being kept alongside the
  output already stored. It is read the same way as the stored output
  (simulation window, daily means, unit conversions). The default, `"aeme"`,
  is as before.
* New dataset `glm_extra_vars`: a registry giving these variables AEME names
  (`SED_temp`, `SED_hflux`, `SED_ztemp`, `SED_Fsed_oxy`, ... for sediment-zone
  variables; `SEDL_Fsed_oxy`, ... for the matching lake-wide series), labels and
  units. Generated from `data-raw/glm_extra_vars.csv`. Either the AEME or the raw
  netCDF name works wherever a variable is looked up: `get_var()`,
  `check_var_in_output()`, `check_aeme_vars()` and `plot_*()`.
* New `extra_vars` argument on `run_aeme()`, `run_aeme_ensemble()`,
  `load_output()`, `read_model_outputs()` and `read_glm_output()` to choose which
  of those variables are read: `"none"` (the default for the first three) reads
  none, `"all"` reads every variable in the file, or give a vector of names
  and/or the keywords `"sediment"` / `"diag"`. Variables not requested are never
  read from `output.nc`. `run_aeme()` and `run_aeme_ensemble()` also now expose
  `keep_diag` (default `TRUE`; `FALSE` drops the extras even if requested).
* New `sed_layers` argument to keep only some sediment layers of `SED_temp`, and
  `zone_summary` to add an area-weighted lake-wide series for each sediment-zone
  variable (`<name>_lake`, using the time-varying zone area `zarea`; zones with no
  area are ignored). Naming a series such as `"SED_Fsed_oxy_lake"` in
  `extra_vars` builds just that series.
* `get_var()` gains `zone` and `sed_layer` to subset sediment variables.
* `plot_output()` now draws sediment-zone variables (and any other variable with
  dimensions beyond time and depth) as a line plot with one line per zone and
  layer, one panel per model, instead of erroring; `plot_model_output()` accepts
  the AEME names in `glm_extra_vars` and both take `zone` and `sed_layer`, so
  `plot_output(aeme, "SED_Fsed_oxy", zone = 1)` works like any other variable.
  Axis labels come from the registry. The base-graphics backend, and several
  variables in one call, still give a clear error for these variables.
* `plot_output()` and `check_if_summary()` now find variables held in the `diag`
  / `sediment` sub-lists (for example `SED_Fsed_oxy_lake`) rather than treating
  them as missing.
* `get_output_vars()` labels these variables from the registry.
* Fixed `check_var_in_output()` reporting variables held in the `diag` /
  `sediment` sub-lists as missing, and sediment-zone variables (`*_Z`) requested
  through `vars_sim` being returned as bare arrays rather than labelled grouped
  variables.

## Observations recorded relative to the lake bed

* `add_obs()` gains `depth_ref` (and accepts a `depth_ref` column in `lake`) so an
  observation can be recorded as a height above the lake bed (`"bottom"`), for
  example a logger 1 m above the sediment, instead of a depth below the surface
  (`"surface"`, the default and the usual case). Stored observations keep
  `depth` in metres below the surface, so everything that reads it is
  unaffected. A bottom-referenced observation also stores `height_above_bed` and
  `depth_ref`, and its `depth` is worked out as `level - height` from the lake
  level observations (interpolated in time), or from the full depth of the basin
  with a warning if there are none. Observations that are all
  surface-referenced get no new columns.
* `get_var(use_obs = TRUE)` compares a bottom-referenced observation with the
  model at (the model's own water level - `height_above_bed`) on that date,
  rather than at the depth it was placed at when added. It also no longer fails
  when the observations fall on a single date.
* `ensemble_summary()` now places observations in the reference of the summary
  before matching them to `depths`. With `depth_ref = "bottom"` a surface
  observation is converted to a height above the bed, and with `"surface"` a
  bottom observation to a depth below it, using the lake level observations
  (interpolated in time) or, where there are none, the ensemble median of the
  modelled level. Previously observations were matched to `depths` as depths
  below the surface whatever `depth_ref` was, so with `"bottom"` they were
  misaligned. `score_ensemble()` uses the same observations.
* `get_obs_column_names(include_optional = TRUE)` now also returns `depth_ref`
  and `height_above_bed`.

## `get_var(depth_ref = "bottom")` follows the water level

* **Behaviour change.** `get_var(depth = d, depth_ref = "bottom")` now reads the
  profile `d` metres above the lake bed, i.e. `level - d` metres below the
  surface on each date, using the modelled water level (`LKE_lvlwtr`). It
  previously used the basin's full depth as a fixed datum, so "1 m from the
  bottom" was always read at (full depth - 1) m below the surface whatever the
  water level; the two only agree when the level equals the full depth. With
  levels that move (inflow/outflow, drawdown, or an extended hypsograph) the old
  answer was off by the difference between the level and the full depth, which
  reached about 3 m and over 1 degC in an ensemble test. On a date when the
  level is below `depth` the value is now `NA` rather than the surface value.
  Results from `get_var()`, and from `ensemble_summary(depth_ref = "bottom")`
  which calls it, will change for bottom-referenced depths; surface-referenced
  extraction is unchanged. `write_aeme_parquet(depth_ref = "bottom")` uses the
  same definition.

## `write_aeme_parquet()` can summarise at fixed depths

* AEME stores GLM output on a depth grid scaled to each day's lake level, so
  members whose water level differs report different depths and the summary
  tier, which collapses across members at each stored depth, ended up with one
  or two members per group. `write_aeme_parquet()` gains `depths` (and
  `depth_ref`): each member's profile is linearly interpolated to the requested
  depths on every date before the members are collapsed. A depth outside a
  member's water column on a date is left out for that member, so `n_ens` is the
  number of members that contributed.
* `depth_ref = "surface"` (default) measures `depths` in metres below the water
  surface; `"bottom"` measures them in metres above the lake bed, converted per
  member and date from the member's water level (`LKE_lvlwtr`).
* The summary tier has a new `depth_ref` column (`"surface"` or `"bottom"` for
  depth-resolved variables, `NA` for variables with no depth dimension).
  `output_raw` is unchanged and always holds the stored depths in metres below
  the surface.

## Faster `write_aeme_parquet()` summary tier

* The ensemble summary tier written by `write_aeme_parquet()` is now computed
  without a list-column per group. The standard depth grid is scaled to each
  member's water level, so depths differ between members and a 12-member
  ensemble has about a million summary groups; the old `dplyr::summarise()` over
  them took over a minute (66 s of a 96 s write).
  The same table now takes a second or two (the whole write about 15 s). The
  values are identical to the previous implementation, checked on the full
  ensemble and on edge cases (missing values, single members, ties). New tests
  cover the writer, which had none.

## Ensemble summary and plotting moved from aemetools

* `ensemble_summary()` and `plot_ensemble()` now live in AEME, next to
  `run_aeme_ensemble()` and `write_aeme_parquet()`. `ensemble_summary()` extracts
  each member once and returns per-date `mean`, `sd`, `n` and quantiles with the
  member frame and depth-aligned observations; `plot_ensemble()` draws a ribbon
  or per-member lines from an `Aeme` object or a summary. Observation
  timestamps (stored as UTC `POSIXct`) are reduced to calendar dates to match
  daily output. Requires dplyr >= 1.1.0.
* New `score_ensemble()` verifies an ensemble against the lake observations
  (from an `ensemble_summary()` or an `Aeme` object): coverage of the central
  interval, bias and RMSE of the ensemble mean, spread, spread-skill ratio and
  mean CRPS, per model, variable and depth and pooled over depths. Coverage of
  a small ensemble is biased low even when it is reliable (about
  `(n - 1) * conf_int / (n + 1)` for `n` members); the documentation says how to
  read it.

## GLM v4 outflow weir parameters

* The `&outflow` block written for GLM v4 now includes `crest_width` (6) and
  `crest_factor` (0.61), alongside `outflow_factor`, `seepage` and
  `seepage_rate`. The same two values are set in the bundled `glm4.nml`
  template.

# AEME 0.4.0

## Parameter helpers moved from aemetools

* `zone_ratio_param()`, `zone_offset_param()` and `expand_zone_ratios()` (plus
  `is_zone_ratio()` and `zone_base_name()`) now live in AEME.
  `input_model_parameters()` expands zone-ratio and zone-offset rows itself, so
  parameter sets that use them work with every function that writes parameters,
  including `run_aeme_ensemble()`.
* `encode_param()`, `decode_param()` and `decode_param_full()` now live in AEME.
  The `group/name[index]` format is unchanged. `encode_param()` returns
  `character(0)` for empty input instead of `"/[]"`, and decoding a string that
  is not in the format gives `NA` instead of a wrong group and name.

## `run_aeme_ensemble()` runs supplied parameter sets

* New `run_aeme_ensemble()` runs a model once per supplied parameter set
  (a list of `param` data.frames, or one long data.frame with an `ensemble`
  column) and stores each member as `output(aeme)$ens_001 ... ens_00k`, the
  structure `get_var(ens_n =)` and `write_aeme_parquet()` already use. Members
  that fail are dropped with a warning; `parallel = TRUE` runs each worker in
  its own copy of the model directory. Sampling sets from parameter bounds and
  extracting a PEST++ posterior stay in the aemetools package.
* `run_aeme_ensemble()` also drops members that fail *silently*: GLM exits
  normally when an unphysical parameter set drains the lake, leaving a water
  level at or below zero and then `NA`. With `drop_incomplete = TRUE` (default)
  such members are removed and named in the warning, along with the first error
  message when a member errors.
* New article "Parameter uncertainty with ensembles" (Uncertainty menu) covering
  sampled and supplied ensembles and `write_aeme_parquet()` /
  `read_aeme_parquet()`.

## `estimate_zone_fluxes()` documentation and Tier 2 normalisation

* **Tier 2 (observed-data) adjustment now preserves the lake-wide total.**
  The documentation has always said that only inter-zone ratios are adjusted
  and the lake-wide area-weighted total is preserved, but the `fsed_amm` and
  `fsed_frp` multipliers were applied after the Tier 1 normalisation and not
  re-normalised, so the total drifted whenever observations were available.
  The adjusted fluxes are now re-normalised. Lakes built with observations of
  near-bed NH4 or FRP will see `fsed_amm`/`fsed_frp` zone values change; the
  area-weighted lake average equals the baseline again.
* Documentation corrected: only NH4 and FRP are adjusted from observations
  (the O2 and NO3 adjustments are disabled), the depth scale factor is capped
  at 2 (not 4), the NO3 sign change is at 0.6 (not 0.5) of the maximum depth,
  and zones are the GLM sediment zones of a built model.
* The normalisation is now an internal function, `normalise_zone_flux()`,
  with unit tests.

## Phytoplankton default fixes and a new succession-diagnostic function

Follow-up to an investigation into whether GLM-AED can reproduce genuine
multi-group phytoplankton succession (diatom/green/cyanobacteria), which
surfaced two structural issues in the shipped `aed_phyto_pars.csv` template
(`inst/extdata/aed/aed_phyto_pars.csv`) that affect every lake built from
package defaults, not just the one investigated:

* **`X_nmin`/`X_nmax`/`X_pmin`/`X_pmax` (Droop internal-quota bounds) were
  byte-identical across every phytoplankton group.** Under this template's
  configuration (`simINDynamics`/`simIPDynamics = 2` for every group),
  these bounds -- not `K_N`/`K_P`, which only apply in a rare edge case --
  are the actual nutrient-competition lever, so shipping them identical
  meant no group-level nutrient differentiation was possible regardless of
  what else was tuned. Given modest, literature-informed, stability-tested
  differentiation: cyanobacteria get a wider phosphorus luxury-storage range
  (`X_pmin` 0.001->0.0007, `X_pmax` 0.005->0.009, reflecting documented
  polyphosphate storage in bloom-forming taxa), and diatoms get a tighter,
  more efficient nitrogen/phosphorus economy (`X_nmin` 0.02->0.025, `X_nmax`
  0.07->0.055, `X_pmin` 0.001->0.0012, `X_pmax` 0.005->0.0038, reflecting
  documented low-luxury nutrient uptake in diatoms relative to greens/
  cyanobacteria). Every other group is unchanged.
* **`simSiUptake` was 0 (off) for every group, including `diatom`.** Per
  `aed_phyto_pars.csv`'s own dbase and `aed_bio_utils.F90`'s `phyto_fSi()`,
  this switch gates whether silica limitation is computed at all --
  `fSi` defaults to `1` (unlimited) whenever it's off. A diatom group
  structurally requires silica; shipping the default with this switch off
  meant silica limitation was silently inert for the one group it's
  supposed to apply to. Now `1` for `diatom`, unchanged (`0`) for every
  other group.

(A third candidate fix -- `aed_zooplankton` being absent from a lake's
active `&aed_models` list, which silently zeroes out all top-down grazing
-- turned out to already be correct in this package's own default
`inst/extdata/aed/aed.nml`; the omission found during the investigation was
specific to one external project's lake configuration, not a package
default.)

## `aed_succession_index()`

New exported function computing a system-level diagnostic for whether a
model run shows genuine, recurring, multi-group phytoplankton succession --
computed purely from model output via `get_var()`, no observations
required. Returns three components (`evenness`, `dom_entropy`,
`periodicity`) plus a `composite`; see `?aed_succession_index` for what
each one catches and the two failed designs (raw autocorrelation; naive
mode-matching) that led to the current `match_rate`/`regime_shift`
construction for `periodicity`. Intended as a PEST regularisation
observation (biasing a calibration away from single-group-monopoly or
one-way-drift solutions) or as a standalone diagnostic.

## Daily-mean output for GLM-AED and Simstrat

`set_output_time_step(aeme, frequency, daily_mean = TRUE)` (new argument, also
`time(aeme)$output_daily_mean`) makes every model produce a daily-mean output
stream alongside its raw `frequency` output, so a run can be done at a
sub-daily cadence while the results compared against daily observations are
true daily means rather than instantaneous snapshots -- the behaviour GOTM's
native `output_daily` stream already provided, now available for all three
hydrodynamic models.

* **GOTM-WET** keeps writing its daily means itself; the `output_daily` block
  of `output.yaml` is now restricted to the targeted variables (the
  `model_controls` `simulate` set plus the internals the reader needs) instead
  of every variable.
* **GLM-AED** and **Simstrat** have no native time-averaging, so `run_aeme()`
  averages their sub-daily `output.nc` by calendar day into a companion
  `output_daily.nc` (same structure, one record per day). The raw sub-daily
  file is kept. The companion carries only the variables the reader consumes
  --- the targeted `model_controls` set plus each reader's internals --- so
  for GLM, whose own netCDF cannot be sub-selected, the companion is also
  where the stored output is pruned (its large 4-D `light` / `umean` / wave
  fields are dropped): ~20 variables instead of ~60, and the averaging pass is
  several times faster and smaller.
* `run_aeme()` / `get_var()` read the daily means transparently: when
  `output_daily_mean` is `TRUE` the `output_daily.nc` companion is read if
  present, otherwise the raw `output.nc` is averaged by calendar day on read.
* Model integration is unchanged (`dt` / `Timestep s` are untouched); the cost
  is the extra in-run output I/O and one post-run averaging pass.
* Averaging arbitrary (non-daily) storage cadences is not yet supported.

## Observation dates are POSIXct

The `Date` column of `observations$lake` and `observations$level` is now a
UTC `POSIXct` rather than a `Date`. **Daily observations are anchored at
12:00:00 UTC**, so a daily value sits unambiguously inside its calendar day
when matched against a sub-daily (`POSIXct`) model axis -- midnight is a
day boundary. Genuinely sub-daily observations keep their time-of-day.

* The conversion happens at every entry point: `yaml_to_aeme()`,
  `lake_obs_to_aeme()`, `add_obs()`, `read_aeme_from_files()` and
  `aeme_constructor()` (a non-`Date`/`POSIXct` column is coerced with a
  warning). Objects loaded from an older `.rds` are converted silently by
  `migrate_aeme()` on the first `check_aeme()` / `show()` / `plot()` /
  `build_aeme()`, or explicitly by `upgrade_aeme()`.
* Observation-to-model matching remains a **calendar-day** join throughout
  (`get_var(use_obs = TRUE)`, `assess_model()`, `align_depth_data()`, the
  water-balance surface-temperature and level fits, `plot_*` overlays), so
  results are unchanged for daily runs.

## Timezone of input data

The model runs entirely in **UTC**: input timestamps are converted to UTC once,
at ingest; every model gets UTC boundary conditions (GLM's nml `timezone` stays
`0`); output is read back and stored in UTC; and only plots / `summary()` render
in local time. You never manage timezones per model or on the way out.

* **Declare the timezone of your input data once**, via
  `aeme_constructor(tz = ...)`, `new_aeme(tz = ...)`, `build_aeme(tz = ...)` or
  `set_time(tz = ...)`. It is stored as `time$tz` (a concrete Olson name) and
  round-trips through `time.csv`, `aeme.yaml` and `.rds`.
* **Default `tz` is `"UTC"`.** Gridded reanalysis (ERA5, etc.) and model
  conventions are all UTC, so most workflows need nothing. Set a non-UTC zone
  only when your source data really is in local time. Legacy `.rds` objects
  migrate to `time$tz = "UTC"`.
* Timestamps you supply -- `time$start` / `time$stop` and the date columns of
  meteo, inflow, outflow and observation inputs -- are read as wall-clock time
  in `time$tz` and converted to UTC at ingest (`add_met()`, `add_inflows()`,
  `aeme_constructor()`, `yaml_to_aeme()`). Daily (calendar) data is never
  shifted.
* When you declare a **non-UTC** `time$tz`, a date column that a CSV reader
  tagged `"UTC"` (`readr`/`read.csv`'s default) is reinterpreted in your
  declared zone -- `"UTC"` is an unreliable label there. A column carrying a
  genuinely different timezone (e.g. `"America/New_York"`) is always kept as an
  absolute instant.
* `standardise_met()` now **warns when sub-daily `MET_radswd` peaks more than
  3 h from astronomical solar noon** for the lake's longitude -- the usual sign
  that meteo timestamps are in local time rather than UTC.
* Datetime parsing across the model-output readers, `check_time()`,
  `standardise_met()` / `standardise_inflow()`, `aeme_time_axis()` and the
  variable-index helpers is now explicitly UTC and independent of the session
  timezone. `show()` / `summary()` print `time$tz` and render `start` / `stop`
  in it.
* GLM's numeric `timezone` nml field is a solar-geometry parameter, unrelated to
  `time$tz`, and is unchanged.

## Initial conditions

* New **`set_initial_conditions()`** / **`get_initial_conditions()`** expose
  the initial water depth, temperature/salinity profile, and biogeochemical
  water-column pools that `build_aeme()` writes into each model. Values can be
  set once for every model or overridden per model via `model_init`
  (e.g. a GLM-AED-specific temperature profile). The specification is stored
  in `configuration(aeme)$initial_conditions` and resolved per model at build
  time (generic defaults <- per-model overrides); it survives
  `load_configuration()`. Scalar water-quality values fold into
  `model_controls$initial_wc`; depth-resolved (`depth`/`value`) profiles are
  applied to the built GLM-AED / Simstrat directories via the existing
  `set_*_init()` writers.
* New **`assess_spin_up()`** builds and runs an ensemble of
  initial-condition perturbations across a range of spin-up lengths and
  reports how quickly the ensemble spread at the start of the analysis
  period collapses, with an optional recommended spin-up. The summary gives
  per-depth-then-averaged `spread` / `drift` in the variable's units plus
  dimensionless `spread_cv` / `drift_cv` (normalised by the ensemble mean),
  and `metric` selects which the recommendation targets. **`plot_spin_up()`**
  plots the chosen metric against spin-up length.

## Sub-daily (hourly) forcing and output

* `time` gains an **`output_time_step`** element (seconds; default `86400`,
  i.e. daily) separate from the integration `time_step`. `set_time()` gains
  `time_step` / `output_time_step` arguments, and both round-trip through
  `time.csv`, `aeme.yaml`, and `.rds` (objects/files without the new field
  default to daily).
* Supplying **sub-daily meteo / inflow** (a `POSIXct` `Date` column, or any
  sub-daily-spaced series) is now carried through `build_aeme()` at its
  native resolution instead of being collapsed to daily, and written to
  GLM-AED (`subdaily`/`nsave`, full timestamps), GOTM-WET
  (`output.yaml` cadence, real time-of-day, `time/dt`), and Simstrat
  (`Output.Times`) accordingly. Set `output_time_step` (e.g. `3600`) to get
  sub-daily output; model output is read back as `POSIXct` when it carries a
  time-of-day and as `Date` (unchanged) when it does not.
* AEME does **not** temporally disaggregate forcing: sub-daily runs require
  forcing supplied at (at least) that cadence, and `build_aeme()` aborts
  otherwise. DYRESM-CAEDYM and the water-balance path remain daily.
* Reading output back no longer silently discards **everything** when the
  reconstructed output time axis is longer than the records a run actually
  wrote (e.g. an hourly `output_time_step` set on a run still written
  daily, or a `stop` that is not an exact multiple of the output step). The
  model readers now keep the overlapping steps and warn, and
  `get_date_index()` trims the index to the file (and warns) when given a
  `path`/`lake_dir`. `get_var(use_obs = TRUE)` against sub-daily output now
  matches one model step per observation day (nearest midnight) instead of
  joining each daily observation to all 24 hourly steps.
* GLM computes its `daily_*` diagnostics -- surface energy fluxes,
  evaporation, surface area/temperature, lake level, in/out/overflow
  volumes -- once per simulated day, so on a sub-daily `read_glm_output()`
  they previously came back `NA` at ~23 of every 24 steps (and any variable
  derived from them, e.g. `LKE_evprte`, `HYD_surft`). Each day's single
  written value is now carried across that day's sub-daily steps; daily
  runs are unaffected.
* The plotting functions now handle a sub-daily (`POSIXct`) output axis:
  `plot_output()` / `plot_model_output()` / `plot_glm_output()` heat-map
  tiles are drawn at the real output step instead of collapsing to
  1-second slivers; `plot_output(backend = "base")` no longer errors on
  the non-unique `as.Date()` axis; observation overlays (`align_depth_data()`)
  match daily observations to the model day rather than an exact timestamp;
  and axis date labels adapt to the plotted span.

## New model

* Added **Simstrat-AED2** (`"simstrat_aed2"`) as a fourth supported model,
  alongside DYRESM-CAEDYM, GLM-AED, and GOTM-WET, with the full
  `build_aeme()`/`run_aeme()`/output-reading pipeline: `build_simstrat()`,
  `run_simstrat_aed2()`, `read_simstrat_output()`, `check_simstrat_par()`,
  `write_simstrat_nc()`, and AED2 biogeochemistry support via
  `initialise_aed2()`.
* Added **Simstrat-AED** (`"simstrat_aed"`) as a fifth supported model,
  coupling Simstrat to AED (v3) instead of AED2 - the same actively
  developed biogeochemical library GLM-AED already links, rather than the
  older AED2. Shares the module-activation/cross-module-dependency logic
  with GLM-AED via a new common engine (`resolve_aed_active_modules()` in
  `R/aed_modules.R`) instead of a second independent copy, so the two AED
  couplings behave identically for the same `model_controls`. New
  `run_simstrat_aed()`, `initialise_simstrat_aed()`,
  `install_simstrat_aed()`/`list_simstrat_aed_versions()`/
  `simstrat_aed_exe_path()`, and `simstrat_aed_parameters` dataset;
  `build_simstrat()` gained a `bgc_lib = c("aed2", "aed")` argument and now
  shares its AED config templates (`inst/extdata/aed/`) with GLM-AED rather
  than each model carrying its own copy.
* Simstrat inflow handling reworked so it can carry nutrient/heat **load**
  like GLM-AED. `make_inf_simstrat()` now:
  - converts AED/AED2 inflow concentrations to the model's native units via
    `conversion_aed` (the same conversion `make_inf_glm()` applies) -
    previously written unconverted;
  - merges multiple inflow streams with a **flow-weighted mean** of every
    concentration-like quantity (`HYD_temp`, `CHM_salt`, each BGC var)
    instead of summing the BGC concentrations, so the single combined
    series carries the same total load as GLM-AED's per-stream inflows;
  - gained the `AEME.simstrat_inflow_load` option (`"none"` (default) /
    `"bgc"` / `"all"`) controlling whether `Tinp.dat`/`Sinp.dat` and the
    AED inflow files are written depth-integrated (effective) or
    single-point (inert). `"none"` keeps the pre-0.4.x behaviour. `"bgc"`
    makes the inflow nutrient load effective. `"all"` also advects inflow
    temperature/salinity but is **experimental** - it currently produces an
    unphysical warm surface bias because Simstrat does not plunge a surface
    point source the way GLM does. When effective, the advected scalar is
    forced to `0` on dates with negligible inflow.
* Simstrat-AED benthic zones are now sized to the lake. When `use_bgc` is on
  and the `simstrat.par` `AEDConfig` block runs a zoned benthic mode
  (`BenthicMode = 2`), `build_simstrat()` sets `NZones` / `ZoneHeights` from
  the hypsography via the same `estimate_sed_zones()` helper `build_glm()`
  uses for GLM-AED, instead of leaving the template's hard-coded values.
  Falls back to the template values if the estimate can't be computed.

## Non-cohesive sediment (`aed_noncohesive`)

* The bundled AED template (`inst/extdata/aed/aed.nml`) now ships an
  `&aed_noncohesive` block with **two** suspended-sediment groups
  (`num_ss = 2`), including constant settling and shear-driven
  resuspension defaults, and lists `aed_noncohesive` in `&aed_models`
  immediately after `aed_sedflux`. The `&aed_noncohesive` **block** is
  placed after `&aed_sed_const2d` in the file: libaed reads the module
  namelists in one forward pass without rewinding between `aed_sedflux`
  (which also consumes `&aed_sed_const2d`) and `aed_noncohesive`, so an
  earlier block is never found and GLM aborts with "ERROR reading namelist
  aed_noncohesive".
* Module activation is wired up in `R/aed_modules.R`: the `NCS` variable
  prefix maps to `aed_noncohesive`, so simulating any `NCS_ss*` variable
  now activates the module (it carries no forced cross-module
  dependencies). `set_glm_aed_models()`'s default `aed_models` includes it.
* `&aed_totals` now counts both groups as TSS
  (`TSS_vars = 'NCS_ss1','NCS_ss2'`). `set_aed_totals()` re-derives
  `TSS_vars` / `TSS_varscale` during `build_aeme()` from the active
  `aed_noncohesive` `num_ss` (one `NCS_ss<i>` per group, unit scaling),
  so the totals survive a build.

## Observations schema: single `depth` column

* The lake observations data frame now uses a single required **`depth`**
  column (nominal sampling depth, m positive-down from the surface) in place
  of the `depth_from` / `depth_to` pair. Every analytical consumer already
  collapsed that pair to its midpoint, so this matches how observations were
  actually used. `get_obs_column_names()` returns
  `c("Date", "var_aeme", "depth", "value")`; pass
  `include_optional = TRUE` for the optional columns.
* Two **optional** columns are recognised: `depth_to` (bottom of an
  integrated sample, for provenance - not consumed by AEME core) and `sd`
  (measurement standard deviation, in the variable's units, for 1/sd
  weighting in PEST-style calibration downstream).
* Backward compatible: `add_obs()`, `lake_obs_to_aeme()`, `yaml_to_aeme()`,
  the constructor, and `migrate_aeme()` / `upgrade_aeme()` all accept the
  legacy `depth_from` / `depth_to` layout and collapse it to `depth`
  (interval midpoint) with a one-time deprecation warning. Objects loaded
  from older `.rds` files are migrated automatically on `check_aeme()` /
  `show()` / `plot()`.
* `lake_obs_to_aeme()` gained optional `depth_to_col_name` and `sd_col_name`
  arguments.
* Fixed a latent bug in `input_model_parameters()` that computed a
  half-thickness (`(depth_to - depth_from) / 2`) instead of a midpoint; the
  value was unused and the line has been removed.

## Model binaries moved out of the package

Model executables are no longer bundled inside the package - they're now
downloaded on demand from GitHub release assets into a persistent local
cache, verified against a published SHA256 checksum before use. This keeps
the installed package small and lets binaries be updated independently of
the R package version.

* `install_glm_aed()`, `install_gotm_wet()`, `install_dy_cd()`,
  `install_simstrat_aed2()` — download and verify a specific (or the
  `"latest"`) model executable version for the current platform. Paired
  `list_*_versions()` (what's published, per platform) and `*_exe_path()`
  (locate an already-installed executable) helpers for each model.
  `install_glm_aed()` supports Windows, macOS, and Linux (including bundled
  dylibs on macOS); GOTM-WET, DYRESM-CAEDYM, and Simstrat-AED2 are
  currently Windows-only, matching the platforms binaries have actually
  been built for.
* `install_models()` — convenience wrapper installing the latest available
  version of every model (or a chosen subset) in one call; models with no
  published binary for the current platform (or no release published yet)
  are reported and skipped rather than blocking the others.
* `run_gotm_wet()` and `run_dy_cd()` now resolve their executable the same
  way `run_glm_aed()` already did: an explicit `AEME.gotm_exec`/
  `AEME.dyresm_exec` option, then a requested/previously installed version,
  with a clear error pointing at `install_gotm_wet()`/`install_dy_cd()` if
  nothing is found. `get_gotm_wet_version()` and `get_dy_cd_version()`
  updated to match.
* `inst/extbin/gotm_wet/` and `inst/extbin/dy_cd/` removed from version
  control (still used locally if present - see `.gitignore`).
* GLM version handling reworked to support multiple installed versions side
  by side (switch between them via `glm_exe_path()`/`AEME.glm_version`
  without re-downloading), with corrected OS detection and a GLM version
  passed through correctly to parallel workers in `run_aeme(parallel = TRUE)`.
* `install_glm_aed()`/`list_glm_versions()` now understand a trailing `"+"`
  on a version (e.g. `"4.0.0+"`), selecting the GLM+ (AED+) build published
  as `glm-<os>-<version>+.zip`. The `.github/workflows/build-glm-v4.yaml`
  workflow was rewritten along the lines of `build-simstrat.yaml` to build
  both the regular `glm` and `glm+` binaries (the latter linking the
  private `libaed-riparian`/`-light`/`-dev` modules) via explicit per-
  library `make` steps, instead of AED_Tools' `build_glm.sh` /
  `build_env.inc` / `build_aedlibs.inc`.

## GLM v4 hydrodynamic configuration

* `build_aeme(model = "glm_aed")` now ships and selects a GLM-v4
  hydrodynamic namelist. A `glm4.nml` template was added at
  `inst/extdata/glm_aed/`, and `build_glm()` copies it in (instead of
  always `glm3.nml`) when the pinned/installed GLM binary is v4 — resolved
  via `.preferred_glm_major_version()`, the same priority order
  `find_glm_nml()` uses. Falls back to `glm3.nml` when the version can't be
  determined or no matching template ships.
* The `&sediment` block is now **merged** rather than overwritten when a
  model is (re)built. `make_stg_glm()` still refreshes the zone geometry and
  per-zone parameters AEME derives from the bathymetry (`n_zones`,
  `zone_heights`, `sed_temp_*`, `sed_reflectivity`, `sed_roughness`, ...),
  but preserves the expanded GLM-v4 soil-column heat-model keys a
  `glm4.nml` carries (`sed_heat_model`, `n_sed_layers`, `sed_layer_depth`,
  `sed_vwc`, `sed_spinup_days`, `sed_deep_temp`). Under
  `sed_heat_model = 2`, `sed_heat_Ksoil` / `sed_temp_depth` are left as the
  template's scalars instead of being expanded to per-zone vectors.
* The GLM-v4 `&mass_balance` block is populated from the AED variables that
  are switched on. New internal `set_glm_mass_balance()` fills
  `balance_vars` / `balance_varnum` straight from the `&init_profiles`
  `wq_names` that `initialise_glm()` has just written, so the two lists
  cannot drift apart; with biogeochemistry off, or no qualifying variable,
  it defaults to `balance_varnum = 0` and drops `balance_vars`. Only touches
  the nml when a `&mass_balance` block is already present (i.e. a `glm4.nml`
  template).
* `initialise_glm()` no longer writes the aggregate totals (`NIT_tn`,
  `PHS_tp`, `CAR_toc` → AED diagnostics `TOT_tn`/`TOT_tp`/`TOT_toc`),
  particulate-inorganic pools (`PHS_pip` → `PHS_frp_ads`, `NIT_pin`),
  `PHY_tchla`, or the `NCS_ss*` groups into `&init_profiles` `wq_names` —
  none are GLM-AED water-column state variables, and GLM aborts with
  `Cannot find "<var>" for initial value` (and, on GLM v4, the equivalent
  `... for mass balance output`) when they appear. The exclusion list is
  shared via the new internal `glm_non_state_vars()` and mirrors what
  `initialise_aed()` already drops.
* `build_glm()` now forces `sed_heat_model` back to `1` when
  `use_bgc = FALSE`: GLM v4's dynamic soil-temperature solver
  (`sed_heat_model = 2`, `zZSoilTemp`) is provided by the WQ library and
  GLM aborts with it enabled but no active WQ module. `check_glm_nml()`
  gained a matching validation rule that flags `sed_heat_model = 2` without
  an active `&wq_setup` (`wq_lib = 'aed'`/`'api'`).

## Restricting model output for calibration

* `set_output_vars(aeme, model, vars, mass_balance = TRUE)` — rewrites the
  output section of a model's configuration so only `vars` (mapped to each
  model's own output names via `key_naming`), plus the handful of internals
  AEME always needs to read a result back, are written. Aimed at
  calibration / sensitivity analysis, where the objective uses one or two
  variables but every model otherwise writes its full state every step.
  Per model: **GLM-AED** drops the fixed-depth `WQ_*.csv` point outputs and,
  with `mass_balance = FALSE`, the `&mass_balance` block — the whole-lake
  `lake.csv` is kept, because GLM 4.x only writes the netCDF diagnostic
  scalars (`lake_level`, ...) while that CSV is open; **Simstrat** switches
  off "write everything" and pins the variable list, cutting ~25 `*_out.dat`
  files to a handful; **GOTM-WET** replaces the `/*` all-variables output
  source with an explicit list; **DYRESM-CAEDYM** has a fixed output form
  and is left unchanged. The change is made in memory — call
  `write_configuration()` (or use `build_aeme()`, below) to write it out.
* `build_aeme()` gained `output_vars` and `mass_balance` arguments: when
  `output_vars` is supplied, `build_aeme()` applies `set_output_vars()` to
  every built model and re-writes the trimmed configuration to disk, so a
  lake can be built restricted from the start. `output_vars = NULL` (the
  default) leaves every model writing its full output, unchanged from
  before.

## OS-aware model selection

* `check_model()` gained an `os_valid` argument: when `TRUE`, restricts the
  requested models to ones actually runnable on the current platform
  (DYRESM-CAEDYM, GOTM-WET, and Simstrat-AED2 need Windows; GLM-AED runs
  everywhere), falling back to GLM-AED with an informative message rather
  than failing outright. `run_aeme()` now applies this automatically.
* An `Aeme` object now remembers which model(s) it was last configured
  for (`configuration(aeme)$model`, defaulting to `"glm_aed"`), so
  `list_models(aeme)` reflects the actual configured model set instead of
  always listing every model AEME supports.

## New functions

* `upgrade_aeme()` — upgrades an `Aeme` object saved by an older AEME
  version to the current layout, in idempotent steps, reporting what it
  changed. On top of the per-model backfills (`time$spin_up`,
  `inflows$factor`, `outflows$factor`, `configuration`) it renames the
  legacy `outflows$lvl` / `outflows$outflow_lvl` element to
  `outflows$elevation`, adds the per-model `output` placeholders and an
  integer `n_members`, coerces a legacy `observations$level` tibble to a
  plain data frame, fills scalar `configuration` build defaults from
  `config_defaults()`, and reorders `parameters` columns to
  `param_colnames()` order. It does **not** rebuild model configuration or
  output — rerun `build_aeme()` for those. Stamps
  `configuration$aeme_upgraded` with the installed version.
* `migrate_aeme()` — the silent, idempotent worker behind `upgrade_aeme()`,
  now also covering the `outflows$elevation` rename, `output` placeholders,
  and `observations$level` coercion. Still called automatically by
  `build_aeme()`, `check_aeme()`, `show()`, and `plot()` so older saved
  objects keep working without needing to be rebuilt.

## Bug fixes

* GLM-AED outflow setup wrote a nonsensical `outl_elvs` and coerced
  `bsn_len_outl` / `bsn_wid_outl` to `NA` for lakes whose hypsography
  extends below 0 m, e.g. a lake bed below sea level. Several problems in
  `build_glm()`, all rooted in `-1` being AEME's "not specified" sentinel
  for an outflow `elevation` (`add_outflows()`; `build_aeme()` sets the
  water-balance outflow to `-1`):
  - the sentinel was recognised only via `heights_wdr <= 0`, so for a lake
    bed below 0 m a real request could be swallowed, or the `-1` sentinel
    leak through as a literal elevation;
  - the "not specified" replacement was `init_depth - 1`, a water *depth*
    written into `outl_elvs`, which for a fixed outlet GLM reads as an
    absolute elevation on the hypsography datum - so it fell outside the
    hypsography and `elipse_dims()`'s area lookup returned `NA`;
  - `outlet_type` / `flt_off_sw` were inferred from the *sign* of the
    elevation (`ifelse(heights_wdr < 0, 2, 1)`), so the default `-1`
    sentinel silently produced a floating offtake, for which GLM expects
    `outl_elvs` as a depth below the surface in `[0, depth]` and rejects
    negative values ("above lake surface").
  `build_glm()` now interprets an outflow `elevation` explicitly: the
  `-1` / `NA` sentinel means *not specified* and defaults to a **floating
  offtake** (`outlet_type = 2`, `flt_off_sw = .true.`) drawing ~1 m above
  the bed but tracking the surface, with `outl_elvs` written as the
  depth-below-surface GLM requires for a floating outlet; any explicit
  `elevation` is placed as a **fixed** outlet at that absolute elevation,
  validated and clamped into `[base_elev, crest_elev]`. Basin length/width
  are looked up at the outlet's true absolute elevation (`make_wdr_glm()`
  gained a `dims_elev` argument for this) and clamped into
  `range(bathy$elev)` so a `set_glm_outflows()` call with an out-of-range
  outlet still yields finite dimensions. For lakes whose hypsography sits
  at/above 0 m the generated `&outflow` block is unchanged; sub-sea-level
  lakes previously failed and now build.

* `set_glm_outflow_config()` (new, exported) — fine-grained control of the
  GLM-AED `&outflow` block for an existing configuration, beyond the
  fixed/floating split `set_glm_outflows()` offers: per-outlet
  adaptive/target-temperature (`type 3`) and submerged (`type 6`) outlets,
  `outlet_crit` thresholds, `target_temp`, `withdrTemp_fl`, bed
  `seepage` / `seepage_rate`, weir `crest_width` / `crest_factor`,
  `outflow_thick_limit` and `single_layer_draw`, plus an `adaptive` list
  for the block-level `crit_*` controls. Elevations are given on the
  hypsography datum and converted per outlet; every value is checked
  against GLM's own ranges (from `src/glm_init.c`) before writing. It edits
  only the nml, leaving the flow-forcing CSVs alone.

* `run_aeme()` reported a successful run and then failed with an opaque
  netCDF error (`open_nc_safe()`: "File path must be a single character
  string") when a model crashed. Three causes: GLM-AED is built with
  gfortran, whose `STOP "..."` (e.g. a malformed `aed_noncohesive`
  namelist) exits `0`, so `run_aeme()`'s `status == 0` success test
  passed; the `verbose = TRUE` path of `run_glm_aed()`/`run_gotm_wet()`
  checked nothing at all, leaving a misleading `cli` tick; and a failed
  model was still handed to `load_output()`. Now each `run_*()` attaches a
  logical `$success` (exit status *and* the model's completion banner, and
  for Simstrat the netCDF conversion), `run_aeme()` gates the "Model run
  complete!" message and output loading on it, and `read_model_outputs()`
  aborts with a clear `aeme_error_missing_output` ("re-run with
  `verbose = TRUE`") when the expected output file is absent.

* `cli_inform_safe()` and `cli_safe()` did not forward an evaluation
  environment to `cli`, so any message containing a `{}` expression that
  referenced a local variable of the *calling* function failed with
  `object '<name>' not found` — `cli` was interpolating against the wrapper's
  own frame. This surfaced when building any `Aeme` object whose inflow
  tables still used pre-standard column names (e.g. `NIT_din`), where
  `standardise_inflow()` reports `"Renaming {length(matched)} column{?s}"`.
  Both wrappers now take `.envir = parent.frame()` and pass it through.

* `read_model_config()` assumed any `.par` configuration file was
  Simstrat's JSON format, but DYRESM-CAEDYM's `dyresm3p1.par` shares that
  extension and is plain text - `read_model_config(model = "dy_cd", ...)`
  failed on any lake with a DYRESM-CAEDYM configuration. Now scoped to
  `model == "simstrat_aed2"` only.
* Fixed a lake-level inversion bug affecting every Simstrat-AED2
  simulation with non-trivial inflows/outflows: `.write_simstrat_grid_file()`
  wrote the two-point depth header used to force a non-zero trapezoidal
  integration in descending order, but Simstrat's `Integrate()` computes
  `dx = x(i) - x(i-1)` directly from the file's own (unreordered) depth
  values. A descending header therefore silently negated every flux this
  writer produces - inflow, outflow, temperature, salinity, and AED2 inflow
  concentrations alike - which is why simulated lake level for
  Simstrat-AED2 tracked in the opposite direction to the other three
  models. Fixed by writing the depths in ascending order; verified by
  comparing Simstrat's own `Qvert` output variable against the expected
  net inflow/outflow signal (now matching almost exactly, correlation
  0.9986, vs. an exact sign-flip before the fix).
* `calc_evap()` gained a dedicated `model == "simstrat_aed2"` branch
  implementing Simstrat's own evaporation formula (a Livingstone & Imboden
  wind function with a Gill (1992) saturation vapour pressure, from
  `strat_forcing.f90`), used by `estimate_lake_wlev()` when fitting the
  water balance for Simstrat-AED2. It previously shared GLM-AED's simpler
  bulk-aerodynamic formula.
* `build_aeme()` did not call `migrate_aeme()`, so an `Aeme` object saved
  before `simstrat_aed2` existed (missing `time$spin_up[["simstrat_aed2"]]`)
  would crash inside `check_time()`'s `compute_spinup_dates()` the first
  time it was built with a newer AEME version. `build_aeme()` now migrates
  the object on entry, matching `check_aeme()`/`show()`/`plot()`.
* `write_simstrat_nc()` mishandled AED's sediment-zone output
  (`<var>_zone_out.dat`, Simstrat-AED only). These files also match the
  general `*_out.dat` glob and were being written against the shared
  water-column `z` dimension/grid used by regular depth-profile variables,
  which either put zone values at the wrong depths or failed outright
  whenever a zone file's column count (one per benthic zone) didn't happen
  to match the water column's level count. Zone variables now get their own
  `zone` netCDF dimension, coordinate-valued by each zone's reference depth
  - keeping the existing `<var>` and new `<var>_zone` variables distinct.
  No changes were needed on the reading side: `read_simstrat_output()`'s
  `load_all` sweep already routes any variable shaped other than `(time)`
  or `(z, time)` through the same generic grouped-variable path GLM-AED's
  own `nzones`-dimensioned output uses, so zone variables come back as
  `aeme_grouped_var` objects automatically. Verified against a real
  Simstrat-AED Rotorua run (75 sediment-zone variables across 3 zones).

## New data

* `simstrat_aed2_parameter_library` — a comprehensive reference table of
  Simstrat-AED2 parameters (physical parameters from the Simstrat User
  Manual, plus AED2 biogeochemical parameters shared with
  `glm_aed_parameter_library`), mirroring the existing
  `glm_aed_parameter_library` dataset.

## Documentation

* Added the `simstrat-aed2` article (`vignettes/articles/simstrat-aed2.Rmd`),
  covering Simstrat-AED2's model description, AED2 module coupling, the new
  parameter library, model-specific features (automatic AED2 module
  selection, inflow modes, ice/snow, water balance fitting), and
  calibration (the `simstrat_aed2_parameters` dataset and Simstrat's native
  PEST-based workflow), mirroring the existing `glm-aed` article.

## Testing

* Added `tests/testthat/helper-glm.R` and `setup.R` with shared helpers for
  skipping/filtering tests by platform availability
  (`skip_if_models_unavailable()`, `filter_platform_models()`,
  `skip_if_no_glm()`) and CI coverage extended to macOS and Ubuntu, in
  addition to Windows.

## `get_model_outfile()` returns a single file by default

`get_model_outfile()` previously returned every file a model run produced --
for GLM-AED that meant its netCDF plus any configured `csv_lake`/`csv_point`/
mass-balance CSVs -- so callers that only wanted the netCDF had to guess
which entry it was (`nc_files[["output"]]`, else the first element), and a
couple did so incorrectly. `get_model_outfile()` now returns just the primary
file per model by default (the entry named `"output"`, or the only file when
there is one); pass `all = TRUE` to get every file as before.

# AEME 0.3.1

## New functions

* `get_config_value()` — retrieve a configuration value from an `Aeme` object,
  falling back to package defaults when not set.
* `get_wbal_param()` — retrieve the fitted water-balance outflow parameters
  (`C` and `h_inv`) stored in an `Aeme` object.
* `set_wbal_param()` — store water-balance outflow parameters in an `Aeme`
  object for use in subsequent `build_aeme()` calls.
* `plot_output_base()` — new base-R plotting function for AEME model output,
  producing heatmap-style depth–time plots without any `ggplot2` dependency.
  Can also be invoked via `plot_output(..., backend = "base")`.
* `get_mean_sea_level_pressure()` — utility function to convert station
  pressure to mean sea level pressure given air temperature and elevation.
* `get_station_pressure()` — inverse of `get_mean_sea_level_pressure()`;
  converts mean sea level pressure back to station pressure.

## Improvements

* **`plot_output()`** — new `backend` argument (`"ggplot2"` or `"base"`)
  selects the plotting engine. Also improved variable/model availability checks:
  models or variables missing from all output are now warned and dropped
  gracefully rather than erroring.
* **`run_aeme()` / `run_glm_aed()`** — new `args` parameter passes additional
  command-line arguments to the model executable (e.g. `"--xdisp"` to display
  GLM plots using the `plots.nml` settings during a run).
* **`build_glm()`** — automatically copies the bundled `plots.nml` template
  into the GLM simulation directory when one is not already present.
* **`build_aeme()`** — `model` and `path` are now resolved from the `Aeme`
  configuration when not supplied as arguments. All other `build_aeme()`
  arguments default through `config_defaults()` if not specified.
* **`estimate_zone_fluxes()`** — output table now rendered using `clitable`
  for cleaner formatted console display; documentation expanded with method
  description, depth-scaling references, and a full description of the return
  value.
* **`initialise_aed()`** — informative message now only shown when an
  initialisation value differs meaningfully from the replaced default.
* **`initialise_glm()`** — added guard for required `init_profiles` fields
  (`wq_names`, `num_wq_vars`, `wq_init_vals`) that may be absent from older
  NML files.
* **`set_glm_aed_models()`** — messaging improved when AED sub-models are
  removed.
* **`run_glm_aed_diagnostics()`** — `plot` argument now defaults to `FALSE`.
* **`calc_water_balance()` / `estimate_lake_wlev()` / `estimate_surface_temperature()` / `standardise_inflow()` / `build_glm()`** — console messaging overhauled using the new `cli_safe()` internal helper, which respects the `AEME.inform` option and supports indented output.  Missing inflow state variables are now reported with their filled default values rather than a generic warning.
* **`clitable`** moved from `Suggests` to `Imports`; `knitr` moved from
  `Imports` to `Suggests`; `psychrolib` removed from `Imports` (psychrometric
  calculations now handled internally via `get_mean_sea_level_pressure()` /
  `get_station_pressure()`).

## Bug fixes

* Fixed variable naming for DY-CD model output (`read_dy_output()`,
  `read_model_outputs()`).
* Removed pH from default model controls (`get_model_controls()`).
* Simplified and corrected variable-name look-ups in `lake_obs_to_aeme()`,
  `read_dy_output()`, `read_glm_output()`, `read_gotm_output()`, and
  `read_model_outputs()` to use the updated `key_naming$var_aeme` column.
* Removed a now-redundant internal helper `format_model_vars_vec()`; its
  behaviour is folded into `get_model_vars(as_vector = TRUE)`.
* Fixed initialisation guard in `initialise_FABM()`, `initialise_aed()`, and
  `initialise_glm()` — empty model-controls tables now return early with an
  informative message rather than erroring.
* Fixed a CLI bug in `estimate_lake_wlev()`.
* Fixed a typo bug in `estimate_zone_fluxes()` column-renaming step
  (`dplyr::case_match` replaced with `dplyr::recode` for compatibility).

# AEME 0.3.0

## New features

* Fixed bug for initialising GLM-AED water column with the values from the 
  model_controls dataframe. This is also added to the GLM .nml file.

## Breaking changes

* `key_naming$name` has been renamed to `key_naming$var_aeme` to align with
  `model_controls$var_aeme` and simplify joins between the two dataframes.
  Update any code that references `key_naming$name` directly.

# AEME 0.2.0

## New functions

* `add_outflows()` — add outflow data to an AEME object.
* `add_output()` — add model output to an AEME object.
* `add_deriv_output()` — compute and attach derived variables (thermocline depth, stratification, Schmidt stability, TLI components, oxygen metrics) to model output.
* `check_model()` — validate that a model name is supported.
* `check_gotm_yaml()` — validate a GOTM YAML configuration file.
* `check_path()` — helper to verify that a file/directory path exists.
* `check_utils()` — miscellaneous input-checking utilities.
* `convert_do()` — convert dissolved-oxygen values between units (mg/L ↔ % saturation).
* `deriv_registry()` — registry of derived-variable definitions used by `add_deriv_output()`.
* `estimate_lake_wlev()` — estimate lake water level from inflow/outflow data and a hypsograph.
* `estimate_sed_zones()` — estimate sediment zones from a hypsograph.
* `estimate_surface_temperature()` — estimate lake surface temperature.
* `estimate_zone_fluxes()` — estimate sediment-zone fluxes for AED models.
* `get_aed_sed_const2d_param()` — retrieve 2-D sediment constant parameters from an AED configuration.
* `get_aeme_path()` — return the path to the AEME package installation.
* `get_date_index()` — return time-step indices for a given date range.
* `get_deriv_vars()` — list available derived variables.
* `get_glm_sed_params()` — retrieve GLM sediment parameters.
* `get_glm_sed_zones()` — retrieve GLM sediment zone definitions.
* `get_hm_palette()` — return the built-in heat-map colour palette.
* `get_vars_sim()` — get the list of variables to simulate.
* `get_wbal_components()` — extract water-balance component data from model output.
* `glm_sed_params()` — construct a GLM sediment-parameter object.
* `glm_to_aeme()` — convert a GLM NML file to an AEME configuration object.
* `guess_aeme_vars()` — auto-detect AEME variable names from a data frame.
* `initialise_aed()` — initialise AED2 configuration (replaces `initialiseAED()`).
* `is_strat()` — test whether a water column is stratified.
* `palettes` — built-in colour palettes for AEME plots.
* `param_colnames()` — return expected column names for the parameter table.
* `plot_est_wbal()` / `plot_water_balance()` — plot estimated water-balance components.
* `plot_glm_config()` — plot GLM model configuration diagnostics.
* `plot_glm_diagnostics()` — plot GLM-AED diagnostic outputs.
* `plot_wbal_comp()` — plot water-balance component comparison across models.
* `plot_wbal_summaries()` — plot summarised water-balance results.
* `plot_weir_calibration()` — plot weir calibration results.
* `plot_wlev()` — plot simulated water-level time series.
* `read_aed_param_csv()` — read AED parameter values from a CSV file.
* `read_dy_output()`, `read_dy_stg()` — read DYRESCD/DY-CD model output and stage files.
* `read_glm_output()` — read GLM model output NetCDF files.
* `read_gotm_flux_output()`, `read_gotm_hyps()`, `read_gotm_output()` — read GOTM output files.
* `read_model_config()` — read a model configuration file.
* `read_model_hypsograph()` — read a hypsograph from a model configuration.
* `read_model_nc()` — low-level reader for model NetCDF files.
* `read_model_outputs()` — unified reader for all supported model outputs.
* `read_model_wlev()` — read water-level output from any supported model.
* `reset_wbal_param()` — reset water-balance parameters to defaults.
* `resolve_dependencies()` — resolve derived-variable computation dependencies.
* `run_glm_aed_diagnostics()` — run GLM-AED post-run diagnostics.
* `set_aed_sed_const2d()` — set 2-D AED sediment constant parameters.
* `set_aed_totals()` — set AED total-concentration parameters.
* `set_glm_aed_models()` — configure which AED sub-models are active.
* `set_vars_sim()` — set the variables to be output by a simulation.
* `standardise_inflow()` — standardise inflow data columns and units.
* `standardise_met()` — standardise meteorological data columns and units.

## Improvements and changes

* **AEME S4 object overhaul** — the `Aeme` S4 class has been substantially refactored: accessor/replacement generics have been simplified and slot structure updated for consistency.
* **`build_aeme()`** — major refactor; improved handling of precipitation, inflows, outflows, and lake-level initialisation.
* **`calc_fairall()`** — vectorised and performance-improved implementation of the Fairall bulk aerodynamic algorithm.
* **`calc_water_balance()`** — refactored water-balance calculation; improved precipitation and evaporation handling.
* **`calc_lake_obs_deriv()`** — extended to compute additional derived observation variables.
* **`expand_met()`** — improved meteorological gap-filling and unit conversion logic.
* **`load_output()`** — now dispatches to the new unified `read_model_outputs()` functions.
* **`check_aeme_vars()`**, **`check_glm_nml()`** — improved validation logic and error messages.
* **`check_time()`** — more robust time-period validation.
* **`print()` / `show()` / `summary()`** — AEME object print methods migrated to use `{cli}` for formatted console output.
* **Inflow/outflow handling** — new auto-detection of column names and units via `standardise_inflow()` and `guess_aeme_vars()`; `add_inflows()` and `add_outflows()` updated accordingly.
* **Rain and snow units** — standardised to mm/day throughout the package.
* **GLM NML helpers** — new tests and improved handling of sediment parameters and zone detection.
* **GOTM YAML** — new `check_gotm_yaml()` validates GOTM configuration before model run.
* **AED initialisation** — `initialiseAED()` replaced by `initialise_aed()`; library name spelling corrected to "Ecodynamics".
* **GLM binaries** — bundled GLM-AED binaries removed from `inst/extbin/`; users now provide their own executable or use the `glmtools` package option.
* **Parameter library** — internal parameter library (`bc2e234`) and `get_aeme_params()` updated to include new parameters.
* **pkgdown site** — favicon assets and extra CSS added; GLM-AED vignette registered.
* Removed deprecated `nc_listify()` internal helper.
* Updated Roxygen documentation to v8 conventions throughout.

## New vignettes

* **GLM-AED** (`vignettes/articles/glm-aed.Rmd`) — end-to-end walkthrough of running GLM coupled with AED2.
* **Reservoir AEME** (`vignettes/articles/reservoir-aeme.Rmd`) — demonstration of AEME on a reservoir.

## Bug fixes

* Fixed phytoplankton group index when building AED configurations (#`45559f4`).
* Fixed outflow renaming bug in `build_aeme()` (#`04838b0`).
* Fixed inflow name update logic in `build_glm()` (#`5c0eb58`).
* Fixed water-balance weir/outflow handling when only the water-balance outflow is present (#`af39204`).
* Fixed `guess_aeme_vars()` guessing logic for edge cases (#`9e9e622`).
* Fixed phytoplankton defaults in AED initialisation (#`c9e78d4`).
* Fixed precipitation unit parsing for percentage values (#`16bcf0b`).
* Corrected air-pressure unit conversion in meteorological processing.

# AEME 0.1.1

* Potential first succesful release on Zenodo

# AEME 0.1.0

* First release on Zenodo

