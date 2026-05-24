# Changelog

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

### Improvements

- **[`build_aeme()`](https://limnotrack.com/reference/build_aeme.md)** —
  `model` and `path` are now resolved from the `Aeme` configuration when
  not supplied as arguments. All other
  [`build_aeme()`](https://limnotrack.com/reference/build_aeme.md)
  arguments default through `config_defaults()` if not specified.
- **[`estimate_zone_fluxes()`](https://limnotrack.com/reference/estimate_zone_fluxes.md)**
  — output table now rendered using `clitable` for cleaner formatted
  console display; messaging improved throughout.
- **`initialise_aed()`** — informative message now only shown when an
  initialisation value differs meaningfully from the replaced default.
- **`initialise_glm()`** — added guard for required `init_profiles`
  fields (`wq_names`, `num_wq_vars`, `wq_init_vals`) that may be absent
  from older NML files.
- **[`set_glm_aed_models()`](https://limnotrack.com/reference/set_glm_aed_models.md)**
  — messaging improved when AED sub-models are removed.
- **`clitable`** moved from `Suggests` to `Imports`; `knitr` moved from
  `Imports` to `Suggests`.

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

## AEME 0.3.0

### New features

- Added `edit_model_controls()`, an interactive Shiny gadget for editing
  model controls in a spreadsheet-like interface. Includes filtering by
  variable name and simulated variables, type-safe editing, and
  validation on save to warn when simulated variables are missing
  initial values.

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
