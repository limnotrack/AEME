# Changelog

## AEME 0.4.0

### `estimate_zone_fluxes()` documentation and Tier 2 normalisation

- **Tier 2 (observed-data) adjustment now preserves the lake-wide
  total.** The documentation has always said that only inter-zone ratios
  are adjusted and the lake-wide area-weighted total is preserved, but
  the `fsed_amm` and `fsed_frp` multipliers were applied after the Tier
  1 normalisation and not re-normalised, so the total drifted whenever
  observations were available. The adjusted fluxes are now
  re-normalised. Lakes built with observations of near-bed NH4 or FRP
  will see `fsed_amm`/`fsed_frp` zone values change; the area-weighted
  lake average equals the baseline again.
- Documentation corrected: only NH4 and FRP are adjusted from
  observations (the O2 and NO3 adjustments are disabled), the depth
  scale factor is capped at 2 (not 4), the NO3 sign change is at 0.6
  (not 0.5) of the maximum depth, and zones are the GLM sediment zones
  of a built model.
- The normalisation is now an internal function,
  `normalise_zone_flux()`, with unit tests.

### Phytoplankton default fixes and a new succession-diagnostic function

Follow-up to an investigation into whether GLM-AED can reproduce genuine
multi-group phytoplankton succession (diatom/green/cyanobacteria), which
surfaced two structural issues in the shipped `aed_phyto_pars.csv`
template (`inst/extdata/aed/aed_phyto_pars.csv`) that affect every lake
built from package defaults, not just the one investigated:

- **`X_nmin`/`X_nmax`/`X_pmin`/`X_pmax` (Droop internal-quota bounds)
  were byte-identical across every phytoplankton group.** Under this
  template’s configuration (`simINDynamics`/`simIPDynamics = 2` for
  every group), these bounds – not `K_N`/`K_P`, which only apply in a
  rare edge case – are the actual nutrient-competition lever, so
  shipping them identical meant no group-level nutrient differentiation
  was possible regardless of what else was tuned. Given modest,
  literature-informed, stability-tested differentiation: cyanobacteria
  get a wider phosphorus luxury-storage range (`X_pmin` 0.001-\>0.0007,
  `X_pmax` 0.005-\>0.009, reflecting documented polyphosphate storage in
  bloom-forming taxa), and diatoms get a tighter, more efficient
  nitrogen/phosphorus economy (`X_nmin` 0.02-\>0.025, `X_nmax`
  0.07-\>0.055, `X_pmin` 0.001-\>0.0012, `X_pmax` 0.005-\>0.0038,
  reflecting documented low-luxury nutrient uptake in diatoms relative
  to greens/ cyanobacteria). Every other group is unchanged.
- **`simSiUptake` was 0 (off) for every group, including `diatom`.** Per
  `aed_phyto_pars.csv`’s own dbase and `aed_bio_utils.F90`’s
  `phyto_fSi()`, this switch gates whether silica limitation is computed
  at all – `fSi` defaults to `1` (unlimited) whenever it’s off. A diatom
  group structurally requires silica; shipping the default with this
  switch off meant silica limitation was silently inert for the one
  group it’s supposed to apply to. Now `1` for `diatom`, unchanged (`0`)
  for every other group.

(A third candidate fix – `aed_zooplankton` being absent from a lake’s
active `&aed_models` list, which silently zeroes out all top-down
grazing – turned out to already be correct in this package’s own default
`inst/extdata/aed/aed.nml`; the omission found during the investigation
was specific to one external project’s lake configuration, not a package
default.)

### `aed_succession_index()`

New exported function computing a system-level diagnostic for whether a
model run shows genuine, recurring, multi-group phytoplankton succession
– computed purely from model output via
[`get_var()`](https://limnotrack.com/reference/get_var.md), no
observations required. Returns three components (`evenness`,
`dom_entropy`, `periodicity`) plus a `composite`; see
[`?aed_succession_index`](https://limnotrack.com/reference/aed_succession_index.md)
for what each one catches and the two failed designs (raw
autocorrelation; naive mode-matching) that led to the current
`match_rate`/`regime_shift` construction for `periodicity`. Intended as
a PEST regularisation observation (biasing a calibration away from
single-group-monopoly or one-way-drift solutions) or as a standalone
diagnostic.

### Daily-mean output for GLM-AED and Simstrat

`set_output_time_step(aeme, frequency, daily_mean = TRUE)` (new
argument, also `time(aeme)$output_daily_mean`) makes every model produce
a daily-mean output stream alongside its raw `frequency` output, so a
run can be done at a sub-daily cadence while the results compared
against daily observations are true daily means rather than
instantaneous snapshots – the behaviour GOTM’s native `output_daily`
stream already provided, now available for all three hydrodynamic
models.

- **GOTM-WET** keeps writing its daily means itself; the `output_daily`
  block of `output.yaml` is now restricted to the targeted variables
  (the `model_controls` `simulate` set plus the internals the reader
  needs) instead of every variable.
- **GLM-AED** and **Simstrat** have no native time-averaging, so
  [`run_aeme()`](https://limnotrack.com/reference/run_aeme.md) averages
  their sub-daily `output.nc` by calendar day into a companion
  `output_daily.nc` (same structure, one record per day). The raw
  sub-daily file is kept. The companion carries only the variables the
  reader consumes — the targeted `model_controls` set plus each reader’s
  internals — so for GLM, whose own netCDF cannot be sub-selected, the
  companion is also where the stored output is pruned (its large 4-D
  `light` / `umean` / wave fields are dropped): ~20 variables instead of
  ~60, and the averaging pass is several times faster and smaller.
- [`run_aeme()`](https://limnotrack.com/reference/run_aeme.md) /
  [`get_var()`](https://limnotrack.com/reference/get_var.md) read the
  daily means transparently: when `output_daily_mean` is `TRUE` the
  `output_daily.nc` companion is read if present, otherwise the raw
  `output.nc` is averaged by calendar day on read.
- Model integration is unchanged (`dt` / `Timestep s` are untouched);
  the cost is the extra in-run output I/O and one post-run averaging
  pass.
- Averaging arbitrary (non-daily) storage cadences is not yet supported.

### Observation dates are POSIXct

The `Date` column of `observations$lake` and `observations$level` is now
a UTC `POSIXct` rather than a `Date`. **Daily observations are anchored
at 12:00:00 UTC**, so a daily value sits unambiguously inside its
calendar day when matched against a sub-daily (`POSIXct`) model axis –
midnight is a day boundary. Genuinely sub-daily observations keep their
time-of-day.

- The conversion happens at every entry point:
  [`yaml_to_aeme()`](https://limnotrack.com/reference/yaml_to_aeme.md),
  [`lake_obs_to_aeme()`](https://limnotrack.com/reference/lake_obs_to_aeme.md),
  [`add_obs()`](https://limnotrack.com/reference/add_obs.md),
  [`read_aeme_from_files()`](https://limnotrack.com/reference/read_aeme_from_files.md)
  and
  [`aeme_constructor()`](https://limnotrack.com/reference/aeme_constructor.md)
  (a non-`Date`/`POSIXct` column is coerced with a warning). Objects
  loaded from an older `.rds` are converted silently by `migrate_aeme()`
  on the first
  [`check_aeme()`](https://limnotrack.com/reference/check_aeme.md) /
  [`show()`](https://rdrr.io/r/methods/show.html) /
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) /
  [`build_aeme()`](https://limnotrack.com/reference/build_aeme.md), or
  explicitly by
  [`upgrade_aeme()`](https://limnotrack.com/reference/upgrade_aeme.md).
- Observation-to-model matching remains a **calendar-day** join
  throughout (`get_var(use_obs = TRUE)`,
  [`assess_model()`](https://limnotrack.com/reference/assess_aeme.md),
  [`align_depth_data()`](https://limnotrack.com/reference/align_depth_data.md),
  the water-balance surface-temperature and level fits, `plot_*`
  overlays), so results are unchanged for daily runs.

### Timezone of input data

The model runs entirely in **UTC**: input timestamps are converted to
UTC once, at ingest; every model gets UTC boundary conditions (GLM’s nml
`timezone` stays `0`); output is read back and stored in UTC; and only
plots / [`summary()`](https://rdrr.io/r/base/summary.html) render in
local time. You never manage timezones per model or on the way out.

- **Declare the timezone of your input data once**, via
  `aeme_constructor(tz = ...)`, `new_aeme(tz = ...)`,
  `build_aeme(tz = ...)` or `set_time(tz = ...)`. It is stored as
  `time$tz` (a concrete Olson name) and round-trips through `time.csv`,
  `aeme.yaml` and `.rds`.
- **Default `tz` is `"UTC"`.** Gridded reanalysis (ERA5, etc.) and model
  conventions are all UTC, so most workflows need nothing. Set a non-UTC
  zone only when your source data really is in local time. Legacy `.rds`
  objects migrate to `time$tz = "UTC"`.
- Timestamps you supply – `time$start` / `time$stop` and the date
  columns of meteo, inflow, outflow and observation inputs – are read as
  wall-clock time in `time$tz` and converted to UTC at ingest
  ([`add_met()`](https://limnotrack.com/reference/add_met.md),
  [`add_inflows()`](https://limnotrack.com/reference/add_inflows.md),
  [`aeme_constructor()`](https://limnotrack.com/reference/aeme_constructor.md),
  [`yaml_to_aeme()`](https://limnotrack.com/reference/yaml_to_aeme.md)).
  Daily (calendar) data is never shifted.
- When you declare a **non-UTC** `time$tz`, a date column that a CSV
  reader tagged `"UTC"` (`readr`/`read.csv`’s default) is reinterpreted
  in your declared zone – `"UTC"` is an unreliable label there. A column
  carrying a genuinely different timezone (e.g. `"America/New_York"`) is
  always kept as an absolute instant.
- [`standardise_met()`](https://limnotrack.com/reference/standardise_met.md)
  now **warns when sub-daily `MET_radswd` peaks more than 3 h from
  astronomical solar noon** for the lake’s longitude – the usual sign
  that meteo timestamps are in local time rather than UTC.
- Datetime parsing across the model-output readers, `check_time()`,
  [`standardise_met()`](https://limnotrack.com/reference/standardise_met.md)
  /
  [`standardise_inflow()`](https://limnotrack.com/reference/standardise_inflow.md),
  `aeme_time_axis()` and the variable-index helpers is now explicitly
  UTC and independent of the session timezone.
  [`show()`](https://rdrr.io/r/methods/show.html) /
  [`summary()`](https://rdrr.io/r/base/summary.html) print `time$tz` and
  render `start` / `stop` in it.
- GLM’s numeric `timezone` nml field is a solar-geometry parameter,
  unrelated to `time$tz`, and is unchanged.

### Initial conditions

- New
  **[`set_initial_conditions()`](https://limnotrack.com/reference/set_initial_conditions.md)**
  /
  **[`get_initial_conditions()`](https://limnotrack.com/reference/get_initial_conditions.md)**
  expose the initial water depth, temperature/salinity profile, and
  biogeochemical water-column pools that
  [`build_aeme()`](https://limnotrack.com/reference/build_aeme.md)
  writes into each model. Values can be set once for every model or
  overridden per model via `model_init` (e.g. a GLM-AED-specific
  temperature profile). The specification is stored in
  `configuration(aeme)$initial_conditions` and resolved per model at
  build time (generic defaults \<- per-model overrides); it survives
  [`load_configuration()`](https://limnotrack.com/reference/load_configuration.md).
  Scalar water-quality values fold into `model_controls$initial_wc`;
  depth-resolved (`depth`/`value`) profiles are applied to the built
  GLM-AED / Simstrat directories via the existing `set_*_init()`
  writers.
- New
  **[`assess_spin_up()`](https://limnotrack.com/reference/assess_spin_up.md)**
  builds and runs an ensemble of initial-condition perturbations across
  a range of spin-up lengths and reports how quickly the ensemble spread
  at the start of the analysis period collapses, with an optional
  recommended spin-up. The summary gives per-depth-then-averaged
  `spread` / `drift` in the variable’s units plus dimensionless
  `spread_cv` / `drift_cv` (normalised by the ensemble mean), and
  `metric` selects which the recommendation targets.
  **[`plot_spin_up()`](https://limnotrack.com/reference/plot_spin_up.md)**
  plots the chosen metric against spin-up length.

### Sub-daily (hourly) forcing and output

- `time` gains an **`output_time_step`** element (seconds; default
  `86400`, i.e. daily) separate from the integration `time_step`.
  [`set_time()`](https://limnotrack.com/reference/set_time.md) gains
  `time_step` / `output_time_step` arguments, and both round-trip
  through `time.csv`, `aeme.yaml`, and `.rds` (objects/files without the
  new field default to daily).
- Supplying **sub-daily meteo / inflow** (a `POSIXct` `Date` column, or
  any sub-daily-spaced series) is now carried through
  [`build_aeme()`](https://limnotrack.com/reference/build_aeme.md) at
  its native resolution instead of being collapsed to daily, and written
  to GLM-AED (`subdaily`/`nsave`, full timestamps), GOTM-WET
  (`output.yaml` cadence, real time-of-day, `time/dt`), and Simstrat
  (`Output.Times`) accordingly. Set `output_time_step` (e.g. `3600`) to
  get sub-daily output; model output is read back as `POSIXct` when it
  carries a time-of-day and as `Date` (unchanged) when it does not.
- AEME does **not** temporally disaggregate forcing: sub-daily runs
  require forcing supplied at (at least) that cadence, and
  [`build_aeme()`](https://limnotrack.com/reference/build_aeme.md)
  aborts otherwise. DYRESM-CAEDYM and the water-balance path remain
  daily.
- Reading output back no longer silently discards **everything** when
  the reconstructed output time axis is longer than the records a run
  actually wrote (e.g. an hourly `output_time_step` set on a run still
  written daily, or a `stop` that is not an exact multiple of the output
  step). The model readers now keep the overlapping steps and warn, and
  [`get_date_index()`](https://limnotrack.com/reference/get_date_index.md)
  trims the index to the file (and warns) when given a
  `path`/`lake_dir`. `get_var(use_obs = TRUE)` against sub-daily output
  now matches one model step per observation day (nearest midnight)
  instead of joining each daily observation to all 24 hourly steps.
- GLM computes its `daily_*` diagnostics – surface energy fluxes,
  evaporation, surface area/temperature, lake level, in/out/overflow
  volumes – once per simulated day, so on a sub-daily
  [`read_glm_output()`](https://limnotrack.com/reference/read_glm_output.md)
  they previously came back `NA` at ~23 of every 24 steps (and any
  variable derived from them, e.g. `LKE_evprte`, `HYD_surft`). Each
  day’s single written value is now carried across that day’s sub-daily
  steps; daily runs are unaffected.
- The plotting functions now handle a sub-daily (`POSIXct`) output axis:
  [`plot_output()`](https://limnotrack.com/reference/plot_output.md) /
  [`plot_model_output()`](https://limnotrack.com/reference/plot_model_output.md)
  /
  [`plot_glm_output()`](https://limnotrack.com/reference/plot_glm_output.md)
  heat-map tiles are drawn at the real output step instead of collapsing
  to 1-second slivers; `plot_output(backend = "base")` no longer errors
  on the non-unique [`as.Date()`](https://rdrr.io/r/base/as.Date.html)
  axis; observation overlays
  ([`align_depth_data()`](https://limnotrack.com/reference/align_depth_data.md))
  match daily observations to the model day rather than an exact
  timestamp; and axis date labels adapt to the plotted span.

### New model

- Added **Simstrat-AED2** (`"simstrat_aed2"`) as a fourth supported
  model, alongside DYRESM-CAEDYM, GLM-AED, and GOTM-WET, with the full
  [`build_aeme()`](https://limnotrack.com/reference/build_aeme.md)/[`run_aeme()`](https://limnotrack.com/reference/run_aeme.md)/output-reading
  pipeline: `build_simstrat()`,
  [`run_simstrat_aed2()`](https://limnotrack.com/reference/run_dy_cd.md),
  [`read_simstrat_output()`](https://limnotrack.com/reference/read_simstrat_output.md),
  [`check_simstrat_par()`](https://limnotrack.com/reference/check_simstrat_par.md),
  [`write_simstrat_nc()`](https://limnotrack.com/reference/write_simstrat_nc.md),
  and AED2 biogeochemistry support via `initialise_aed2()`.
- Added **Simstrat-AED** (`"simstrat_aed"`) as a fifth supported model,
  coupling Simstrat to AED (v3) instead of AED2 - the same actively
  developed biogeochemical library GLM-AED already links, rather than
  the older AED2. Shares the module-activation/cross-module-dependency
  logic with GLM-AED via a new common engine
  (`resolve_aed_active_modules()` in `R/aed_modules.R`) instead of a
  second independent copy, so the two AED couplings behave identically
  for the same `model_controls`. New
  [`run_simstrat_aed()`](https://limnotrack.com/reference/run_dy_cd.md),
  `initialise_simstrat_aed()`,
  [`install_simstrat_aed()`](https://limnotrack.com/reference/install_simstrat_aed.md)/[`list_simstrat_aed_versions()`](https://limnotrack.com/reference/list_simstrat_aed_versions.md)/
  [`simstrat_aed_exe_path()`](https://limnotrack.com/reference/simstrat_aed_exe_path.md),
  and `simstrat_aed_parameters` dataset; `build_simstrat()` gained a
  `bgc_lib = c("aed2", "aed")` argument and now shares its AED config
  templates (`inst/extdata/aed/`) with GLM-AED rather than each model
  carrying its own copy.
- Simstrat inflow handling reworked so it can carry nutrient/heat
  **load** like GLM-AED. `make_inf_simstrat()` now:
  - converts AED/AED2 inflow concentrations to the model’s native units
    via `conversion_aed` (the same conversion `make_inf_glm()`
    applies) - previously written unconverted;
  - merges multiple inflow streams with a **flow-weighted mean** of
    every concentration-like quantity (`HYD_temp`, `CHM_salt`, each BGC
    var) instead of summing the BGC concentrations, so the single
    combined series carries the same total load as GLM-AED’s per-stream
    inflows;
  - gained the `AEME.simstrat_inflow_load` option (`"none"` (default) /
    `"bgc"` / `"all"`) controlling whether `Tinp.dat`/`Sinp.dat` and the
    AED inflow files are written depth-integrated (effective) or
    single-point (inert). `"none"` keeps the pre-0.4.x behaviour.
    `"bgc"` makes the inflow nutrient load effective. `"all"` also
    advects inflow temperature/salinity but is **experimental** - it
    currently produces an unphysical warm surface bias because Simstrat
    does not plunge a surface point source the way GLM does. When
    effective, the advected scalar is forced to `0` on dates with
    negligible inflow.
- Simstrat-AED benthic zones are now sized to the lake. When `use_bgc`
  is on and the `simstrat.par` `AEDConfig` block runs a zoned benthic
  mode (`BenthicMode = 2`), `build_simstrat()` sets `NZones` /
  `ZoneHeights` from the hypsography via the same
  [`estimate_sed_zones()`](https://limnotrack.com/reference/estimate_sed_zones.md)
  helper `build_glm()` uses for GLM-AED, instead of leaving the
  template’s hard-coded values. Falls back to the template values if the
  estimate can’t be computed.

### Non-cohesive sediment (`aed_noncohesive`)

- The bundled AED template (`inst/extdata/aed/aed.nml`) now ships an
  `&aed_noncohesive` block with **two** suspended-sediment groups
  (`num_ss = 2`), including constant settling and shear-driven
  resuspension defaults, and lists `aed_noncohesive` in `&aed_models`
  immediately after `aed_sedflux`. The `&aed_noncohesive` **block** is
  placed after `&aed_sed_const2d` in the file: libaed reads the module
  namelists in one forward pass without rewinding between `aed_sedflux`
  (which also consumes `&aed_sed_const2d`) and `aed_noncohesive`, so an
  earlier block is never found and GLM aborts with “ERROR reading
  namelist aed_noncohesive”.
- Module activation is wired up in `R/aed_modules.R`: the `NCS` variable
  prefix maps to `aed_noncohesive`, so simulating any `NCS_ss*` variable
  now activates the module (it carries no forced cross-module
  dependencies).
  [`set_glm_aed_models()`](https://limnotrack.com/reference/set_glm_aed_models.md)’s
  default `aed_models` includes it.
- `&aed_totals` now counts both groups as TSS
  (`TSS_vars = 'NCS_ss1','NCS_ss2'`).
  [`set_aed_totals()`](https://limnotrack.com/reference/set_aed_totals.md)
  re-derives `TSS_vars` / `TSS_varscale` during
  [`build_aeme()`](https://limnotrack.com/reference/build_aeme.md) from
  the active `aed_noncohesive` `num_ss` (one `NCS_ss<i>` per group, unit
  scaling), so the totals survive a build.

### Observations schema: single `depth` column

- The lake observations data frame now uses a single required
  **`depth`** column (nominal sampling depth, m positive-down from the
  surface) in place of the `depth_from` / `depth_to` pair. Every
  analytical consumer already collapsed that pair to its midpoint, so
  this matches how observations were actually used.
  [`get_obs_column_names()`](https://limnotrack.com/reference/get_obs_column_names.md)
  returns `c("Date", "var_aeme", "depth", "value")`; pass
  `include_optional = TRUE` for the optional columns.
- Two **optional** columns are recognised: `depth_to` (bottom of an
  integrated sample, for provenance - not consumed by AEME core) and
  `sd` (measurement standard deviation, in the variable’s units, for
  1/sd weighting in PEST-style calibration downstream).
- Backward compatible:
  [`add_obs()`](https://limnotrack.com/reference/add_obs.md),
  [`lake_obs_to_aeme()`](https://limnotrack.com/reference/lake_obs_to_aeme.md),
  [`yaml_to_aeme()`](https://limnotrack.com/reference/yaml_to_aeme.md),
  the constructor, and `migrate_aeme()` /
  [`upgrade_aeme()`](https://limnotrack.com/reference/upgrade_aeme.md)
  all accept the legacy `depth_from` / `depth_to` layout and collapse it
  to `depth` (interval midpoint) with a one-time deprecation warning.
  Objects loaded from older `.rds` files are migrated automatically on
  [`check_aeme()`](https://limnotrack.com/reference/check_aeme.md) /
  [`show()`](https://rdrr.io/r/methods/show.html) /
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).
- [`lake_obs_to_aeme()`](https://limnotrack.com/reference/lake_obs_to_aeme.md)
  gained optional `depth_to_col_name` and `sd_col_name` arguments.
- Fixed a latent bug in
  [`input_model_parameters()`](https://limnotrack.com/reference/input_model_parameters.md)
  that computed a half-thickness (`(depth_to - depth_from) / 2`) instead
  of a midpoint; the value was unused and the line has been removed.

### Model binaries moved out of the package

Model executables are no longer bundled inside the package - they’re now
downloaded on demand from GitHub release assets into a persistent local
cache, verified against a published SHA256 checksum before use. This
keeps the installed package small and lets binaries be updated
independently of the R package version.

- [`install_glm_aed()`](https://limnotrack.com/reference/install_glm_aed.md),
  [`install_gotm_wet()`](https://limnotrack.com/reference/install_gotm_wet.md),
  [`install_dy_cd()`](https://limnotrack.com/reference/install_dy_cd.md),
  [`install_simstrat_aed2()`](https://limnotrack.com/reference/install_simstrat_aed2.md)
  — download and verify a specific (or the `"latest"`) model executable
  version for the current platform. Paired `list_*_versions()` (what’s
  published, per platform) and `*_exe_path()` (locate an
  already-installed executable) helpers for each model.
  [`install_glm_aed()`](https://limnotrack.com/reference/install_glm_aed.md)
  supports Windows, macOS, and Linux (including bundled dylibs on
  macOS); GOTM-WET, DYRESM-CAEDYM, and Simstrat-AED2 are currently
  Windows-only, matching the platforms binaries have actually been built
  for.
- [`install_models()`](https://limnotrack.com/reference/install_models.md)
  — convenience wrapper installing the latest available version of every
  model (or a chosen subset) in one call; models with no published
  binary for the current platform (or no release published yet) are
  reported and skipped rather than blocking the others.
- [`run_gotm_wet()`](https://limnotrack.com/reference/run_dy_cd.md) and
  [`run_dy_cd()`](https://limnotrack.com/reference/run_dy_cd.md) now
  resolve their executable the same way
  [`run_glm_aed()`](https://limnotrack.com/reference/run_dy_cd.md)
  already did: an explicit `AEME.gotm_exec`/ `AEME.dyresm_exec` option,
  then a requested/previously installed version, with a clear error
  pointing at
  [`install_gotm_wet()`](https://limnotrack.com/reference/install_gotm_wet.md)/[`install_dy_cd()`](https://limnotrack.com/reference/install_dy_cd.md)
  if nothing is found. `get_gotm_wet_version()` and
  `get_dy_cd_version()` updated to match.
- `inst/extbin/gotm_wet/` and `inst/extbin/dy_cd/` removed from version
  control (still used locally if present - see `.gitignore`).
- GLM version handling reworked to support multiple installed versions
  side by side (switch between them via
  [`glm_exe_path()`](https://limnotrack.com/reference/glm_exe_path.md)/`AEME.glm_version`
  without re-downloading), with corrected OS detection and a GLM version
  passed through correctly to parallel workers in
  `run_aeme(parallel = TRUE)`.
- [`install_glm_aed()`](https://limnotrack.com/reference/install_glm_aed.md)/[`list_glm_versions()`](https://limnotrack.com/reference/list_glm_versions.md)
  now understand a trailing `"+"` on a version (e.g. `"4.0.0+"`),
  selecting the GLM+ (AED+) build published as
  `glm-<os>-<version>+.zip`. The `.github/workflows/build-glm-v4.yaml`
  workflow was rewritten along the lines of `build-simstrat.yaml` to
  build both the regular `glm` and `glm+` binaries (the latter linking
  the private `libaed-riparian`/`-light`/`-dev` modules) via explicit
  per- library `make` steps, instead of AED_Tools’ `build_glm.sh` /
  `build_env.inc` / `build_aedlibs.inc`.

### GLM v4 hydrodynamic configuration

- `build_aeme(model = "glm_aed")` now ships and selects a GLM-v4
  hydrodynamic namelist. A `glm4.nml` template was added at
  `inst/extdata/glm_aed/`, and `build_glm()` copies it in (instead of
  always `glm3.nml`) when the pinned/installed GLM binary is v4 —
  resolved via `.preferred_glm_major_version()`, the same priority order
  [`find_glm_nml()`](https://limnotrack.com/reference/find_glm_nml.md)
  uses. Falls back to `glm3.nml` when the version can’t be determined or
  no matching template ships.
- The `&sediment` block is now **merged** rather than overwritten when a
  model is (re)built. `make_stg_glm()` still refreshes the zone geometry
  and per-zone parameters AEME derives from the bathymetry (`n_zones`,
  `zone_heights`, `sed_temp_*`, `sed_reflectivity`, `sed_roughness`, …),
  but preserves the expanded GLM-v4 soil-column heat-model keys a
  `glm4.nml` carries (`sed_heat_model`, `n_sed_layers`,
  `sed_layer_depth`, `sed_vwc`, `sed_spinup_days`, `sed_deep_temp`).
  Under `sed_heat_model = 2`, `sed_heat_Ksoil` / `sed_temp_depth` are
  left as the template’s scalars instead of being expanded to per-zone
  vectors.
- The GLM-v4 `&mass_balance` block is populated from the AED variables
  that are switched on. New internal `set_glm_mass_balance()` fills
  `balance_vars` / `balance_varnum` straight from the `&init_profiles`
  `wq_names` that `initialise_glm()` has just written, so the two lists
  cannot drift apart; with biogeochemistry off, or no qualifying
  variable, it defaults to `balance_varnum = 0` and drops
  `balance_vars`. Only touches the nml when a `&mass_balance` block is
  already present (i.e. a `glm4.nml` template).
- `initialise_glm()` no longer writes the aggregate totals (`NIT_tn`,
  `PHS_tp`, `CAR_toc` → AED diagnostics `TOT_tn`/`TOT_tp`/`TOT_toc`),
  particulate-inorganic pools (`PHS_pip` → `PHS_frp_ads`, `NIT_pin`),
  `PHY_tchla`, or the `NCS_ss*` groups into `&init_profiles` `wq_names`
  — none are GLM-AED water-column state variables, and GLM aborts with
  `Cannot find "<var>" for initial value` (and, on GLM v4, the
  equivalent `... for mass balance output`) when they appear. The
  exclusion list is shared via the new internal `glm_non_state_vars()`
  and mirrors what `initialise_aed()` already drops.
- `build_glm()` now forces `sed_heat_model` back to `1` when
  `use_bgc = FALSE`: GLM v4’s dynamic soil-temperature solver
  (`sed_heat_model = 2`, `zZSoilTemp`) is provided by the WQ library and
  GLM aborts with it enabled but no active WQ module.
  [`check_glm_nml()`](https://limnotrack.com/reference/check_glm_nml.md)
  gained a matching validation rule that flags `sed_heat_model = 2`
  without an active `&wq_setup` (`wq_lib = 'aed'`/`'api'`).

### Restricting model output for calibration

- `set_output_vars(aeme, model, vars, mass_balance = TRUE)` — rewrites
  the output section of a model’s configuration so only `vars` (mapped
  to each model’s own output names via `key_naming`), plus the handful
  of internals AEME always needs to read a result back, are written.
  Aimed at calibration / sensitivity analysis, where the objective uses
  one or two variables but every model otherwise writes its full state
  every step. Per model: **GLM-AED** drops the fixed-depth `WQ_*.csv`
  point outputs and, with `mass_balance = FALSE`, the `&mass_balance`
  block — the whole-lake `lake.csv` is kept, because GLM 4.x only writes
  the netCDF diagnostic scalars (`lake_level`, …) while that CSV is
  open; **Simstrat** switches off “write everything” and pins the
  variable list, cutting ~25 `*_out.dat` files to a handful;
  **GOTM-WET** replaces the `/*` all-variables output source with an
  explicit list; **DYRESM-CAEDYM** has a fixed output form and is left
  unchanged. The change is made in memory — call
  [`write_configuration()`](https://limnotrack.com/reference/write_configuration.md)
  (or use
  [`build_aeme()`](https://limnotrack.com/reference/build_aeme.md),
  below) to write it out.
- [`build_aeme()`](https://limnotrack.com/reference/build_aeme.md)
  gained `output_vars` and `mass_balance` arguments: when `output_vars`
  is supplied,
  [`build_aeme()`](https://limnotrack.com/reference/build_aeme.md)
  applies
  [`set_output_vars()`](https://limnotrack.com/reference/set_output_vars.md)
  to every built model and re-writes the trimmed configuration to disk,
  so a lake can be built restricted from the start. `output_vars = NULL`
  (the default) leaves every model writing its full output, unchanged
  from before.

### OS-aware model selection

- [`check_model()`](https://limnotrack.com/reference/check_model.md)
  gained an `os_valid` argument: when `TRUE`, restricts the requested
  models to ones actually runnable on the current platform
  (DYRESM-CAEDYM, GOTM-WET, and Simstrat-AED2 need Windows; GLM-AED runs
  everywhere), falling back to GLM-AED with an informative message
  rather than failing outright.
  [`run_aeme()`](https://limnotrack.com/reference/run_aeme.md) now
  applies this automatically.
- An `Aeme` object now remembers which model(s) it was last configured
  for (`configuration(aeme)$model`, defaulting to `"glm_aed"`), so
  `list_models(aeme)` reflects the actual configured model set instead
  of always listing every model AEME supports.

### New functions

- [`upgrade_aeme()`](https://limnotrack.com/reference/upgrade_aeme.md) —
  upgrades an `Aeme` object saved by an older AEME version to the
  current layout, in idempotent steps, reporting what it changed. On top
  of the per-model backfills (`time$spin_up`, `inflows$factor`,
  `outflows$factor`, `configuration`) it renames the legacy
  `outflows$lvl` / `outflows$outflow_lvl` element to
  `outflows$elevation`, adds the per-model `output` placeholders and an
  integer `n_members`, coerces a legacy `observations$level` tibble to a
  plain data frame, fills scalar `configuration` build defaults from
  `config_defaults()`, and reorders `parameters` columns to
  [`param_colnames()`](https://limnotrack.com/reference/param_colnames.md)
  order. It does **not** rebuild model configuration or output — rerun
  [`build_aeme()`](https://limnotrack.com/reference/build_aeme.md) for
  those. Stamps `configuration$aeme_upgraded` with the installed
  version.
- `migrate_aeme()` — the silent, idempotent worker behind
  [`upgrade_aeme()`](https://limnotrack.com/reference/upgrade_aeme.md),
  now also covering the `outflows$elevation` rename, `output`
  placeholders, and `observations$level` coercion. Still called
  automatically by
  [`build_aeme()`](https://limnotrack.com/reference/build_aeme.md),
  [`check_aeme()`](https://limnotrack.com/reference/check_aeme.md),
  [`show()`](https://rdrr.io/r/methods/show.html), and
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) so older
  saved objects keep working without needing to be rebuilt.

### Bug fixes

- GLM-AED outflow setup wrote a nonsensical `outl_elvs` and coerced
  `bsn_len_outl` / `bsn_wid_outl` to `NA` for lakes whose hypsography
  extends below 0 m, e.g. a lake bed below sea level. Several problems
  in `build_glm()`, all rooted in `-1` being AEME’s “not specified”
  sentinel for an outflow `elevation`
  ([`add_outflows()`](https://limnotrack.com/reference/add_outflows.md);
  [`build_aeme()`](https://limnotrack.com/reference/build_aeme.md) sets
  the water-balance outflow to `-1`):

  - the sentinel was recognised only via `heights_wdr <= 0`, so for a
    lake bed below 0 m a real request could be swallowed, or the `-1`
    sentinel leak through as a literal elevation;
  - the “not specified” replacement was `init_depth - 1`, a water
    *depth* written into `outl_elvs`, which for a fixed outlet GLM reads
    as an absolute elevation on the hypsography datum - so it fell
    outside the hypsography and `elipse_dims()`’s area lookup returned
    `NA`;
  - `outlet_type` / `flt_off_sw` were inferred from the *sign* of the
    elevation (`ifelse(heights_wdr < 0, 2, 1)`), so the default `-1`
    sentinel silently produced a floating offtake, for which GLM expects
    `outl_elvs` as a depth below the surface in `[0, depth]` and rejects
    negative values (“above lake surface”). `build_glm()` now interprets
    an outflow `elevation` explicitly: the `-1` / `NA` sentinel means
    *not specified* and defaults to a **floating offtake**
    (`outlet_type = 2`, `flt_off_sw = .true.`) drawing ~1 m above the
    bed but tracking the surface, with `outl_elvs` written as the
    depth-below-surface GLM requires for a floating outlet; any explicit
    `elevation` is placed as a **fixed** outlet at that absolute
    elevation, validated and clamped into `[base_elev, crest_elev]`.
    Basin length/width are looked up at the outlet’s true absolute
    elevation (`make_wdr_glm()` gained a `dims_elev` argument for this)
    and clamped into `range(bathy$elev)` so a
    [`set_glm_outflows()`](https://limnotrack.com/reference/set_glm_outflows.md)
    call with an out-of-range outlet still yields finite dimensions. For
    lakes whose hypsography sits at/above 0 m the generated `&outflow`
    block is unchanged; sub-sea-level lakes previously failed and now
    build.

- [`set_glm_outflow_config()`](https://limnotrack.com/reference/set_glm_outflow_config.md)
  (new, exported) — fine-grained control of the GLM-AED `&outflow` block
  for an existing configuration, beyond the fixed/floating split
  [`set_glm_outflows()`](https://limnotrack.com/reference/set_glm_outflows.md)
  offers: per-outlet adaptive/target-temperature (`type 3`) and
  submerged (`type 6`) outlets, `outlet_crit` thresholds, `target_temp`,
  `withdrTemp_fl`, bed `seepage` / `seepage_rate`, weir `crest_width` /
  `crest_factor`, `outflow_thick_limit` and `single_layer_draw`, plus an
  `adaptive` list for the block-level `crit_*` controls. Elevations are
  given on the hypsography datum and converted per outlet; every value
  is checked against GLM’s own ranges (from `src/glm_init.c`) before
  writing. It edits only the nml, leaving the flow-forcing CSVs alone.

- [`run_aeme()`](https://limnotrack.com/reference/run_aeme.md) reported
  a successful run and then failed with an opaque netCDF error
  ([`open_nc_safe()`](https://limnotrack.com/reference/open_nc_safe.md):
  “File path must be a single character string”) when a model crashed.
  Three causes: GLM-AED is built with gfortran, whose `STOP "..."`
  (e.g. a malformed `aed_noncohesive` namelist) exits `0`, so
  [`run_aeme()`](https://limnotrack.com/reference/run_aeme.md)’s
  `status == 0` success test passed; the `verbose = TRUE` path of
  [`run_glm_aed()`](https://limnotrack.com/reference/run_dy_cd.md)/[`run_gotm_wet()`](https://limnotrack.com/reference/run_dy_cd.md)
  checked nothing at all, leaving a misleading `cli` tick; and a failed
  model was still handed to
  [`load_output()`](https://limnotrack.com/reference/load_output.md).
  Now each `run_*()` attaches a logical `$success` (exit status *and*
  the model’s completion banner, and for Simstrat the netCDF
  conversion),
  [`run_aeme()`](https://limnotrack.com/reference/run_aeme.md) gates the
  “Model run complete!” message and output loading on it, and
  [`read_model_outputs()`](https://limnotrack.com/reference/read_model_outputs.md)
  aborts with a clear `aeme_error_missing_output` (“re-run with
  `verbose = TRUE`”) when the expected output file is absent.

- [`cli_inform_safe()`](https://limnotrack.com/reference/cli_inform_safe.md)
  and [`cli_safe()`](https://limnotrack.com/reference/cli_safe.md) did
  not forward an evaluation environment to `cli`, so any message
  containing a [`{}`](https://rdrr.io/r/base/Paren.html) expression that
  referenced a local variable of the *calling* function failed with
  `object '<name>' not found` — `cli` was interpolating against the
  wrapper’s own frame. This surfaced when building any `Aeme` object
  whose inflow tables still used pre-standard column names
  (e.g. `NIT_din`), where
  [`standardise_inflow()`](https://limnotrack.com/reference/standardise_inflow.md)
  reports `"Renaming {length(matched)} column{?s}"`. Both wrappers now
  take `.envir = parent.frame()` and pass it through.

- [`read_model_config()`](https://limnotrack.com/reference/read_model_config.md)
  assumed any `.par` configuration file was Simstrat’s JSON format, but
  DYRESM-CAEDYM’s `dyresm3p1.par` shares that extension and is plain
  text - `read_model_config(model = "dy_cd", ...)` failed on any lake
  with a DYRESM-CAEDYM configuration. Now scoped to
  `model == "simstrat_aed2"` only.

- Fixed a lake-level inversion bug affecting every Simstrat-AED2
  simulation with non-trivial inflows/outflows:
  `.write_simstrat_grid_file()` wrote the two-point depth header used to
  force a non-zero trapezoidal integration in descending order, but
  Simstrat’s `Integrate()` computes `dx = x(i) - x(i-1)` directly from
  the file’s own (unreordered) depth values. A descending header
  therefore silently negated every flux this writer produces - inflow,
  outflow, temperature, salinity, and AED2 inflow concentrations alike -
  which is why simulated lake level for Simstrat-AED2 tracked in the
  opposite direction to the other three models. Fixed by writing the
  depths in ascending order; verified by comparing Simstrat’s own
  `Qvert` output variable against the expected net inflow/outflow signal
  (now matching almost exactly, correlation 0.9986, vs. an exact
  sign-flip before the fix).

- `calc_evap()` gained a dedicated `model == "simstrat_aed2"` branch
  implementing Simstrat’s own evaporation formula (a Livingstone &
  Imboden wind function with a Gill (1992) saturation vapour pressure,
  from `strat_forcing.f90`), used by
  [`estimate_lake_wlev()`](https://limnotrack.com/reference/estimate_lake_wlev.md)
  when fitting the water balance for Simstrat-AED2. It previously shared
  GLM-AED’s simpler bulk-aerodynamic formula.

- [`build_aeme()`](https://limnotrack.com/reference/build_aeme.md) did
  not call `migrate_aeme()`, so an `Aeme` object saved before
  `simstrat_aed2` existed (missing `time$spin_up[["simstrat_aed2"]]`)
  would crash inside `check_time()`’s `compute_spinup_dates()` the first
  time it was built with a newer AEME version.
  [`build_aeme()`](https://limnotrack.com/reference/build_aeme.md) now
  migrates the object on entry, matching
  [`check_aeme()`](https://limnotrack.com/reference/check_aeme.md)/[`show()`](https://rdrr.io/r/methods/show.html)/[`plot()`](https://rdrr.io/r/graphics/plot.default.html).

- [`write_simstrat_nc()`](https://limnotrack.com/reference/write_simstrat_nc.md)
  mishandled AED’s sediment-zone output (`<var>_zone_out.dat`,
  Simstrat-AED only). These files also match the general `*_out.dat`
  glob and were being written against the shared water-column `z`
  dimension/grid used by regular depth-profile variables, which either
  put zone values at the wrong depths or failed outright whenever a zone
  file’s column count (one per benthic zone) didn’t happen to match the
  water column’s level count. Zone variables now get their own `zone`
  netCDF dimension, coordinate-valued by each zone’s reference depth

  - keeping the existing `<var>` and new `<var>_zone` variables
    distinct. No changes were needed on the reading side:
    [`read_simstrat_output()`](https://limnotrack.com/reference/read_simstrat_output.md)’s
    `load_all` sweep already routes any variable shaped other than
    `(time)` or `(z, time)` through the same generic grouped-variable
    path GLM-AED’s own `nzones`-dimensioned output uses, so zone
    variables come back as `aeme_grouped_var` objects automatically.
    Verified against a real Simstrat-AED Rotorua run (75 sediment-zone
    variables across 3 zones).

### New data

- `simstrat_aed2_parameter_library` — a comprehensive reference table of
  Simstrat-AED2 parameters (physical parameters from the Simstrat User
  Manual, plus AED2 biogeochemical parameters shared with
  `glm_aed_parameter_library`), mirroring the existing
  `glm_aed_parameter_library` dataset.

### Documentation

- Added the `simstrat-aed2` article
  (`vignettes/articles/simstrat-aed2.Rmd`), covering Simstrat-AED2’s
  model description, AED2 module coupling, the new parameter library,
  model-specific features (automatic AED2 module selection, inflow
  modes, ice/snow, water balance fitting), and calibration (the
  `simstrat_aed2_parameters` dataset and Simstrat’s native PEST-based
  workflow), mirroring the existing `glm-aed` article.

### Testing

- Added `tests/testthat/helper-glm.R` and `setup.R` with shared helpers
  for skipping/filtering tests by platform availability
  (`skip_if_models_unavailable()`, `filter_platform_models()`,
  `skip_if_no_glm()`) and CI coverage extended to macOS and Ubuntu, in
  addition to Windows.

### `get_model_outfile()` returns a single file by default

[`get_model_outfile()`](https://limnotrack.com/reference/get_model_outfile.md)
previously returned every file a model run produced – for GLM-AED that
meant its netCDF plus any configured `csv_lake`/`csv_point`/
mass-balance CSVs – so callers that only wanted the netCDF had to guess
which entry it was (`nc_files[["output"]]`, else the first element), and
a couple did so incorrectly.
[`get_model_outfile()`](https://limnotrack.com/reference/get_model_outfile.md)
now returns just the primary file per model by default (the entry named
`"output"`, or the only file when there is one); pass `all = TRUE` to
get every file as before.

## AEME 0.3.1

### New functions

- [`get_config_value()`](https://limnotrack.com/reference/get_config_value.md)
  — retrieve a configuration value from an `Aeme` object, falling back
  to package defaults when not set.
- [`get_wbal_param()`](https://limnotrack.com/reference/get_wbal_param.md)
  — retrieve the fitted water-balance outflow parameters (`C` and
  `h_inv`) stored in an `Aeme` object.
- [`set_wbal_param()`](https://limnotrack.com/reference/set_wbal_param.md)
  — store water-balance outflow parameters in an `Aeme` object for use
  in subsequent
  [`build_aeme()`](https://limnotrack.com/reference/build_aeme.md)
  calls.
- [`plot_output_base()`](https://limnotrack.com/reference/plot_output_base.md)
  — new base-R plotting function for AEME model output, producing
  heatmap-style depth–time plots without any `ggplot2` dependency. Can
  also be invoked via `plot_output(..., backend = "base")`.
- [`get_mean_sea_level_pressure()`](https://limnotrack.com/reference/get_mean_sea_level_pressure.md)
  — utility function to convert station pressure to mean sea level
  pressure given air temperature and elevation.
- [`get_station_pressure()`](https://limnotrack.com/reference/get_station_pressure.md)
  — inverse of
  [`get_mean_sea_level_pressure()`](https://limnotrack.com/reference/get_mean_sea_level_pressure.md);
  converts mean sea level pressure back to station pressure.

### Improvements

- **[`plot_output()`](https://limnotrack.com/reference/plot_output.md)**
  — new `backend` argument (`"ggplot2"` or `"base"`) selects the
  plotting engine. Also improved variable/model availability checks:
  models or variables missing from all output are now warned and dropped
  gracefully rather than erroring.
- **[`run_aeme()`](https://limnotrack.com/reference/run_aeme.md) /
  [`run_glm_aed()`](https://limnotrack.com/reference/run_dy_cd.md)** —
  new `args` parameter passes additional command-line arguments to the
  model executable (e.g. `"--xdisp"` to display GLM plots using the
  `plots.nml` settings during a run).
- **`build_glm()`** — automatically copies the bundled `plots.nml`
  template into the GLM simulation directory when one is not already
  present.
- **[`build_aeme()`](https://limnotrack.com/reference/build_aeme.md)** —
  `model` and `path` are now resolved from the `Aeme` configuration when
  not supplied as arguments. All other
  [`build_aeme()`](https://limnotrack.com/reference/build_aeme.md)
  arguments default through `config_defaults()` if not specified.
- **[`estimate_zone_fluxes()`](https://limnotrack.com/reference/estimate_zone_fluxes.md)**
  — output table now rendered using `clitable` for cleaner formatted
  console display; documentation expanded with method description,
  depth-scaling references, and a full description of the return value.
- **`initialise_aed()`** — informative message now only shown when an
  initialisation value differs meaningfully from the replaced default.
- **`initialise_glm()`** — added guard for required `init_profiles`
  fields (`wq_names`, `num_wq_vars`, `wq_init_vals`) that may be absent
  from older NML files.
- **[`set_glm_aed_models()`](https://limnotrack.com/reference/set_glm_aed_models.md)**
  — messaging improved when AED sub-models are removed.
- **[`run_glm_aed_diagnostics()`](https://limnotrack.com/reference/run_glm_aed_diagnostics.md)**
  — `plot` argument now defaults to `FALSE`.
- **`calc_water_balance()` /
  [`estimate_lake_wlev()`](https://limnotrack.com/reference/estimate_lake_wlev.md)
  /
  [`estimate_surface_temperature()`](https://limnotrack.com/reference/estimate_surface_temperature.md)
  /
  [`standardise_inflow()`](https://limnotrack.com/reference/standardise_inflow.md)
  / `build_glm()`** — console messaging overhauled using the new
  [`cli_safe()`](https://limnotrack.com/reference/cli_safe.md) internal
  helper, which respects the `AEME.inform` option and supports indented
  output. Missing inflow state variables are now reported with their
  filled default values rather than a generic warning.
- **`clitable`** moved from `Suggests` to `Imports`; `knitr` moved from
  `Imports` to `Suggests`; `psychrolib` removed from `Imports`
  (psychrometric calculations now handled internally via
  [`get_mean_sea_level_pressure()`](https://limnotrack.com/reference/get_mean_sea_level_pressure.md)
  /
  [`get_station_pressure()`](https://limnotrack.com/reference/get_station_pressure.md)).

### Bug fixes

- Fixed variable naming for DY-CD model output
  ([`read_dy_output()`](https://limnotrack.com/reference/read_dy_output.md),
  [`read_model_outputs()`](https://limnotrack.com/reference/read_model_outputs.md)).
- Removed pH from default model controls
  ([`get_model_controls()`](https://limnotrack.com/reference/get_model_controls.md)).
- Simplified and corrected variable-name look-ups in
  [`lake_obs_to_aeme()`](https://limnotrack.com/reference/lake_obs_to_aeme.md),
  [`read_dy_output()`](https://limnotrack.com/reference/read_dy_output.md),
  [`read_glm_output()`](https://limnotrack.com/reference/read_glm_output.md),
  [`read_gotm_output()`](https://limnotrack.com/reference/read_gotm_output.md),
  and
  [`read_model_outputs()`](https://limnotrack.com/reference/read_model_outputs.md)
  to use the updated `key_naming$var_aeme` column.
- Removed a now-redundant internal helper `format_model_vars_vec()`; its
  behaviour is folded into `get_model_vars(as_vector = TRUE)`.
- Fixed initialisation guard in `initialise_FABM()`, `initialise_aed()`,
  and `initialise_glm()` — empty model-controls tables now return early
  with an informative message rather than erroring.
- Fixed a CLI bug in
  [`estimate_lake_wlev()`](https://limnotrack.com/reference/estimate_lake_wlev.md).
- Fixed a typo bug in
  [`estimate_zone_fluxes()`](https://limnotrack.com/reference/estimate_zone_fluxes.md)
  column-renaming step
  ([`dplyr::case_match`](https://dplyr.tidyverse.org/reference/case_match.html)
  replaced with
  [`dplyr::recode`](https://dplyr.tidyverse.org/reference/recode.html)
  for compatibility).

## AEME 0.3.0

### New features

- Fixed bug for initialising GLM-AED water column with the values from
  the model_controls dataframe. This is also added to the GLM .nml file.

### Breaking changes

- `key_naming$name` has been renamed to `key_naming$var_aeme` to align
  with `model_controls$var_aeme` and simplify joins between the two
  dataframes. Update any code that references `key_naming$name`
  directly.

## AEME 0.2.0

### New functions

- [`add_outflows()`](https://limnotrack.com/reference/add_outflows.md) —
  add outflow data to an AEME object.
- [`add_output()`](https://limnotrack.com/reference/add_output.md) — add
  model output to an AEME object.
- [`add_deriv_output()`](https://limnotrack.com/reference/add_deriv_output.md)
  — compute and attach derived variables (thermocline depth,
  stratification, Schmidt stability, TLI components, oxygen metrics) to
  model output.
- [`check_model()`](https://limnotrack.com/reference/check_model.md) —
  validate that a model name is supported.
- [`check_gotm_yaml()`](https://limnotrack.com/reference/check_gotm_yaml.md)
  — validate a GOTM YAML configuration file.
- [`check_path()`](https://limnotrack.com/reference/check_path.md) —
  helper to verify that a file/directory path exists.
- `check_utils()` — miscellaneous input-checking utilities.
- [`convert_do()`](https://limnotrack.com/reference/convert_do.md) —
  convert dissolved-oxygen values between units (mg/L ↔︎ % saturation).
- `deriv_registry()` — registry of derived-variable definitions used by
  [`add_deriv_output()`](https://limnotrack.com/reference/add_deriv_output.md).
- [`estimate_lake_wlev()`](https://limnotrack.com/reference/estimate_lake_wlev.md)
  — estimate lake water level from inflow/outflow data and a hypsograph.
- [`estimate_sed_zones()`](https://limnotrack.com/reference/estimate_sed_zones.md)
  — estimate sediment zones from a hypsograph.
- [`estimate_surface_temperature()`](https://limnotrack.com/reference/estimate_surface_temperature.md)
  — estimate lake surface temperature.
- [`estimate_zone_fluxes()`](https://limnotrack.com/reference/estimate_zone_fluxes.md)
  — estimate sediment-zone fluxes for AED models.
- [`get_aed_sed_const2d_param()`](https://limnotrack.com/reference/get_aed_sed_const2d_param.md)
  — retrieve 2-D sediment constant parameters from an AED configuration.
- [`get_aeme_path()`](https://limnotrack.com/reference/get_aeme_path.md)
  — return the path to the AEME package installation.
- [`get_date_index()`](https://limnotrack.com/reference/get_date_index.md)
  — return time-step indices for a given date range.
- `get_deriv_vars()` — list available derived variables.
- [`get_glm_sed_params()`](https://limnotrack.com/reference/get_glm_sed_params.md)
  — retrieve GLM sediment parameters.
- [`get_glm_sed_zones()`](https://limnotrack.com/reference/get_glm_sed_zones.md)
  — retrieve GLM sediment zone definitions.
- [`get_hm_palette()`](https://limnotrack.com/reference/get_hm_palette.md)
  — return the built-in heat-map colour palette.
- [`get_vars_sim()`](https://limnotrack.com/reference/get_vars_sim.md) —
  get the list of variables to simulate.
- [`get_wbal_components()`](https://limnotrack.com/reference/get_wbal_components.md)
  — extract water-balance component data from model output.
- [`glm_sed_params()`](https://limnotrack.com/reference/glm_sed_params.md)
  — construct a GLM sediment-parameter object.
- [`glm_to_aeme()`](https://limnotrack.com/reference/glm_to_aeme.md) —
  convert a GLM NML file to an AEME configuration object.
- [`guess_aeme_vars()`](https://limnotrack.com/reference/guess_aeme_vars.md)
  — auto-detect AEME variable names from a data frame.
- `initialise_aed()` — initialise AED2 configuration (replaces
  `initialiseAED()`).
- [`is_strat()`](https://limnotrack.com/reference/is_strat.md) — test
  whether a water column is stratified.
- `palettes` — built-in colour palettes for AEME plots.
- [`param_colnames()`](https://limnotrack.com/reference/param_colnames.md)
  — return expected column names for the parameter table.
- [`plot_est_wbal()`](https://limnotrack.com/reference/plot_est_wbal.md)
  / `plot_water_balance()` — plot estimated water-balance components.
- [`plot_glm_config()`](https://limnotrack.com/reference/plot_glm_config.md)
  — plot GLM model configuration diagnostics.
- [`plot_glm_diagnostics()`](https://limnotrack.com/reference/plot_glm_diagnostics.md)
  — plot GLM-AED diagnostic outputs.
- [`plot_wbal_comp()`](https://limnotrack.com/reference/plot_wbal_comp.md)
  — plot water-balance component comparison across models.
- [`plot_wbal_summaries()`](https://limnotrack.com/reference/plot_wbal_summaries.md)
  — plot summarised water-balance results.
- [`plot_weir_calibration()`](https://limnotrack.com/reference/plot_weir_calibration.md)
  — plot weir calibration results.
- [`plot_wlev()`](https://limnotrack.com/reference/plot_wlev.md) — plot
  simulated water-level time series.
- [`read_aed_param_csv()`](https://limnotrack.com/reference/read_aed_param_csv.md)
  — read AED parameter values from a CSV file.
- [`read_dy_output()`](https://limnotrack.com/reference/read_dy_output.md),
  `read_dy_stg()` — read DYRESCD/DY-CD model output and stage files.
- [`read_glm_output()`](https://limnotrack.com/reference/read_glm_output.md)
  — read GLM model output NetCDF files.
- [`read_gotm_flux_output()`](https://limnotrack.com/reference/read_gotm_flux_output.md),
  `read_gotm_hyps()`,
  [`read_gotm_output()`](https://limnotrack.com/reference/read_gotm_output.md)
  — read GOTM output files.
- [`read_model_config()`](https://limnotrack.com/reference/read_model_config.md)
  — read a model configuration file.
- [`read_model_hypsograph()`](https://limnotrack.com/reference/read_model_hypsograph.md)
  — read a hypsograph from a model configuration.
- `read_model_nc()` — low-level reader for model NetCDF files.
- [`read_model_outputs()`](https://limnotrack.com/reference/read_model_outputs.md)
  — unified reader for all supported model outputs.
- [`read_model_wlev()`](https://limnotrack.com/reference/read_model_wlev.md)
  — read water-level output from any supported model.
- [`reset_wbal_param()`](https://limnotrack.com/reference/reset_wbal_param.md)
  — reset water-balance parameters to defaults.
- `resolve_dependencies()` — resolve derived-variable computation
  dependencies.
- [`run_glm_aed_diagnostics()`](https://limnotrack.com/reference/run_glm_aed_diagnostics.md)
  — run GLM-AED post-run diagnostics.
- [`set_aed_sed_const2d()`](https://limnotrack.com/reference/set_aed_sed_const2d.md)
  — set 2-D AED sediment constant parameters.
- [`set_aed_totals()`](https://limnotrack.com/reference/set_aed_totals.md)
  — set AED total-concentration parameters.
- [`set_glm_aed_models()`](https://limnotrack.com/reference/set_glm_aed_models.md)
  — configure which AED sub-models are active.
- [`set_vars_sim()`](https://limnotrack.com/reference/set_vars_sim.md) —
  set the variables to be output by a simulation.
- [`standardise_inflow()`](https://limnotrack.com/reference/standardise_inflow.md)
  — standardise inflow data columns and units.
- [`standardise_met()`](https://limnotrack.com/reference/standardise_met.md)
  — standardise meteorological data columns and units.

### Improvements and changes

- **AEME S4 object overhaul** — the `Aeme` S4 class has been
  substantially refactored: accessor/replacement generics have been
  simplified and slot structure updated for consistency.
- **[`build_aeme()`](https://limnotrack.com/reference/build_aeme.md)** —
  major refactor; improved handling of precipitation, inflows, outflows,
  and lake-level initialisation.
- **`calc_fairall()`** — vectorised and performance-improved
  implementation of the Fairall bulk aerodynamic algorithm.
- **`calc_water_balance()`** — refactored water-balance calculation;
  improved precipitation and evaporation handling.
- **`calc_lake_obs_deriv()`** — extended to compute additional derived
  observation variables.
- **[`expand_met()`](https://limnotrack.com/reference/expand_met.md)** —
  improved meteorological gap-filling and unit conversion logic.
- **[`load_output()`](https://limnotrack.com/reference/load_output.md)**
  — now dispatches to the new unified
  [`read_model_outputs()`](https://limnotrack.com/reference/read_model_outputs.md)
  functions.
- **[`check_aeme_vars()`](https://limnotrack.com/reference/check_aeme_vars.md)**,
  **[`check_glm_nml()`](https://limnotrack.com/reference/check_glm_nml.md)**
  — improved validation logic and error messages.
- **`check_time()`** — more robust time-period validation.
- **[`print()`](https://rdrr.io/r/base/print.html) /
  [`show()`](https://rdrr.io/r/methods/show.html) /
  [`summary()`](https://rdrr.io/r/base/summary.html)** — AEME object
  print methods migrated to use [cli](https://cli.r-lib.org) for
  formatted console output.
- **Inflow/outflow handling** — new auto-detection of column names and
  units via
  [`standardise_inflow()`](https://limnotrack.com/reference/standardise_inflow.md)
  and
  [`guess_aeme_vars()`](https://limnotrack.com/reference/guess_aeme_vars.md);
  [`add_inflows()`](https://limnotrack.com/reference/add_inflows.md) and
  [`add_outflows()`](https://limnotrack.com/reference/add_outflows.md)
  updated accordingly.
- **Rain and snow units** — standardised to mm/day throughout the
  package.
- **GLM NML helpers** — new tests and improved handling of sediment
  parameters and zone detection.
- **GOTM YAML** — new
  [`check_gotm_yaml()`](https://limnotrack.com/reference/check_gotm_yaml.md)
  validates GOTM configuration before model run.
- **AED initialisation** — `initialiseAED()` replaced by
  `initialise_aed()`; library name spelling corrected to “Ecodynamics”.
- **GLM binaries** — bundled GLM-AED binaries removed from
  `inst/extbin/`; users now provide their own executable or use the
  `glmtools` package option.
- **Parameter library** — internal parameter library (`bc2e234`) and
  `get_aeme_params()` updated to include new parameters.
- **pkgdown site** — favicon assets and extra CSS added; GLM-AED
  vignette registered.
- Removed deprecated `nc_listify()` internal helper.
- Updated Roxygen documentation to v8 conventions throughout.

### New vignettes

- **GLM-AED** (`vignettes/articles/glm-aed.Rmd`) — end-to-end
  walkthrough of running GLM coupled with AED2.
- **Reservoir AEME** (`vignettes/articles/reservoir-aeme.Rmd`) —
  demonstration of AEME on a reservoir.

### Bug fixes

- Fixed phytoplankton group index when building AED configurations
  (#`45559f4`).
- Fixed outflow renaming bug in
  [`build_aeme()`](https://limnotrack.com/reference/build_aeme.md)
  (#`04838b0`).
- Fixed inflow name update logic in `build_glm()` (#`5c0eb58`).
- Fixed water-balance weir/outflow handling when only the water-balance
  outflow is present (#`af39204`).
- Fixed
  [`guess_aeme_vars()`](https://limnotrack.com/reference/guess_aeme_vars.md)
  guessing logic for edge cases (#`9e9e622`).
- Fixed phytoplankton defaults in AED initialisation (#`c9e78d4`).
- Fixed precipitation unit parsing for percentage values (#`16bcf0b`).
- Corrected air-pressure unit conversion in meteorological processing.

## AEME 0.1.1

- Potential first succesful release on Zenodo

## AEME 0.1.0

- First release on Zenodo
