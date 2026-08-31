# AEME 0.4.0

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

