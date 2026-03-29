# Package index

## All functions

- [`add_deriv_output()`](add_deriv_output.md) : Add derived output
  variables to model output
- [`add_hum_vars()`](add_hum_vars.md) : Add humidity variables to
  meteorological data frame
- [`add_hypsograph()`](add_hypsograph.md) : Add hypsograph to Aeme
  object
- [`add_inflow()`](add_inflow.md) : Set inflow data from Aeme object
- [`add_inflows()`](add_inflows.md) : Add inflows to Aeme object
- [`add_met()`](add_met.md) : Add meteorological data to Aeme object
- [`add_obs()`](add_obs.md) : Add observations to Aeme object
- [`add_outflows()`](add_outflows.md) : Add outflows to Aeme object
- [`add_output()`](add_output.md) : Add model output to Aeme object
- [`add_param()`](add_param.md) : Add model parameters to Aeme object
- [`aed_phyto_pars`](aed_phyto_pars.md) : Example dataframe of
  parameters for phytoplankton in the GLM-AED model.
- [`Aeme`](aeme.md) [`Aeme-class`](aeme.md) : Aeme Class
- [`aeme_constructor()`](aeme_constructor.md) : Constructor function for
  Aeme class
- [`aeme_parameters`](aeme_parameters.md) : Example dataframe used for
  calibrating AEME models.
- [`aeme_parameters_bgc`](aeme_parameters_bgc.md) : Example dataframe
  used for calibrating the biogeochemistry in the AEME models.
- [`aeme_to_inflow()`](aeme_to_inflow.md) : Convert AEME model outputs
  to inflow format
- [`align_depth_data()`](align_depth_data.md) : Align observation depth
  data with model data
- [`assess_model()`](assess_model.md) : Assess model performance
- [`build_aeme()`](build_aeme.md) : Build model configuration
  directories
- [`calc_humidity_vars()`](calc_humidity_vars.md) : Calculate
  humidity-related variables using GOTM formulas
- [`calc_lake_vol()`](calc_lake_vol.md) : Calculate the volume of a lake
  using bathymetry data or a hypsograph
- [`calc_tli3()`](calc_tli3.md) : Calculate TLI 3
- [`calc_tli4()`](calc_tli4.md) : Calculate TLI 4
- [`calc_tli_chla()`](calc_tli_chla.md) : Calculate TLI chlorophyll
  component
- [`calc_tli_n()`](calc_tli_n.md) : Calculate TLI nitrogen component
- [`calc_tli_p()`](calc_tli_p.md) : Calculate TLI phosphorus component
- [`calc_tli_secchi()`](calc_tli_secchi.md) : Calculate TLI secchi
  component
- [`catchment_yields`](catchment_yields.md) : Catchment yields
- [`check_AEME_pkg()`](check_AEME_pkg.md) : Check if the package is
  working correctly
- [`check_aeme_vars()`](check_aeme_vars.md) : Check AEME variable names
- [`check_glm_nml()`](check_glm_nml.md) : Check GLM nml for common
  issues
- [`check_gotm_yaml()`](check_gotm_yaml.md) : Check GOTM YAML
  configuration file for common issues
- [`check_model()`](check_model.md) : Check model name and return
  standardized code
- [`check_model_output()`](check_model_output.md) : Check model output
- [`check_obs_var()`](check_obs_var.md) : Check if a variable is present
  in the observations for the model time period
- [`check_path()`](check_path.md) : Check and manage file paths
- [`check_var_in_output()`](check_var_in_output.md) : Check if a
  variable is present in the model output
- [`configuration(`*`<Aeme>`*`)`](configuration-aeme-method.md) : Access
  configuration slot
- [`` `configuration<-`( ``*`<Aeme>`*`)`](configuration-set-aeme-method.md)
  : Set configuration in Aeme object
- [`` `configuration<-`() ``](configuration-set.md) : Set configuration
  in Aeme object
- [`configuration()`](configuration.md) : Access configuration slot
- [`convert_do()`](convert_do.md) : Convert dissolved oxygen between
  mg/L and percent saturation
- [`dy_cd_parameters`](dy_cd_parameters.md) : Example dataset of
  parameters for the DYRESM-CAEDYM model
- [`estimate_lake_wlev()`](estimate_lake_wlev.md) : Estimate Lake Water
  Levels with Nudging
- [`estimate_sed_zones()`](estimate_sed_zones.md) : Estimate sediment
  zones based on hypsograph
- [`estimate_surface_temperature()`](estimate_surface_temperature.md) :
  Estimate Surface Temperature Using Energy Balance Model
- [`expand_met()`](expand_met.md) : Expand a minimal set of meteorology
  inputs to a complete set of variables suitable for all models
- [`extrap_hyps()`](extrap_hyps.md) : Extend hypsometry to a greater
  elevation using linear extrapolation
- [`generate_hypsograph()`](generate_hypsograph.md) : Generate a
  hypsograph curve
- [`generate_var_map_code()`](generate_var_map_code.md) : Generate
  variable mapping code for lake observation data
- [`get_aed_sed_const2d_param()`](get_aed_sed_const2d_param.md) : Get
  AED sed_const2d parameters
- [`get_aeme_parameters()`](get_aeme_parameters.md) : Make parameters
  dataframe for AEME
- [`get_aeme_path()`](get_aeme_path.md) : Get the path to the AEME data
- [`get_date_index()`](get_date_index.md) : Get date index for each
  model in the AEME object
- [`get_deriv_inputs()`](get_deriv_inputs.md) : Get derived variables
  needed for simulation
- [`get_glm_sed_params()`](get_glm_sed_params.md) : Get number of
  sediment zones in GLM-AED model
- [`get_glm_sed_zones()`](get_glm_sed_zones.md) : Get number of sediment
  zones in GLM-AED model
- [`get_hm_palette()`](get_hm_palette.md) : Get default heatmap palette
  for a variable
- [`get_hypsograph()`](get_hypsograph.md) : Get hypsograph from Aeme
  object
- [`get_inflows()`](get_inflows.md) : Get inflow data from Aeme object
- [`get_lake()`](get_lake.md) : Get lake data from Aeme object
- [`get_lake_dir()`](get_lake_dir.md) : Get the directory of the lake
  model setup
- [`get_met()`](get_met.md) : Get meteorological data from Aeme object
- [`get_model_config_files()`](get_model_config_files.md) : Get model
  configuration files paths
- [`get_model_controls()`](get_model_controls.md) : Get model controls
- [`get_model_outfile()`](get_model_outfile.md) : Get model output file
- [`get_model_version()`](get_model_version.md) : Get model version
- [`get_mod_obs_vars()`](get_mod_obs_vars.md) : Get modeled observation
  variables
- [`get_nml_value()`](get_nml_value.md) : gets a nml value according to
  an arg_name
- [`get_obs()`](get_obs.md) : Get observations for a given variable from
  an Aeme object
- [`get_obs_column_names()`](get_obs_column_names.md) : Get column names
  for the observational data frame
- [`get_output_vars()`](get_output_vars.md) : Get the output variables
  from an AEME object
- [`get_var()`](get_var.md) : Get variable from aeme
- [`get_vars_sim()`](get_vars_sim.md) : Get all variables to be
  simulated, including those that are derived from others.
- [`get_var_indices()`](get_var_indices.md) : Get variable indices
- [`get_wbal_components()`](get_wbal_components.md) : Get water balance
  components from AEME object
- [`glm_aed_parameters`](glm_aed_parameters.md) : Example dataframe used
  for calibrating the biogeochemistry in the GLM-AED model.
- [`glm_sed_params()`](glm_sed_params.md) : Generate GLM Sediment
  Parameters
- [`glm_to_aeme()`](glm_to_aeme.md) : Load a GLM nml file and convert to
  aeme object
- [`gotm_wet_parameters`](gotm_wet_parameters.md) : Example dataset of
  parameters for the GOTM-WET model
- [`inflows(`*`<Aeme>`*`)`](inflows-aeme-method.md) : Access inflows
  slot
- [`` `inflows<-`( ``*`<Aeme>`*`)`](inflows-set-aeme-method.md) : Set
  inflows in Aeme object
- [`` `inflows<-`() ``](inflows-set.md) : Set inflows in Aeme object
- [`inflows()`](inflows.md) : Access inflows slot
- [`input(`*`<Aeme>`*`)`](input-aeme-method.md) : Access input slot
- [`` `input<-`( ``*`<Aeme>`*`)`](input-set-aeme-method.md) : Set input
  in Aeme object
- [`` `input<-`() ``](input-set.md) : Set input in Aeme object
- [`input()`](input.md) : Access input slot
- [`input_model_parameters()`](input_model_parameters.md) : Input model
  parameters
- [`insert_aeme()`](insert_aeme.md) : Insert default aeme list code
  snippet.
- [`is_model_error()`](is_model_error.md) : Check if object is a model
  output error
- [`is_strat()`](is_strat.md) : Check if water temperature profile is
  stratified
- [`key_naming`](key_naming.md) : Model variable naming
- [`lake(`*`<Aeme>`*`)`](lake-aeme-method.md) : Access lake slot
- [`` `lake<-`( ``*`<Aeme>`*`)`](lake-set-aeme-method.md) : Set lake in
  Aeme object
- [`` `lake<-`() ``](lake-set.md) : Set lake in Aeme object
- [`lake()`](lake.md) : Access lake slot
- [`lake_obs_to_aeme()`](lake_obs_to_aeme.md) : Format lake observation
  data to AEME format
- [`list_models()`](list_models.md) : Get a vector of available models
- [`list_mod_obs_vars()`](list_mod_obs_vars.md) : Get the variables that
  are both in the observation and model output
- [`list_obs_vars()`](list_obs_vars.md) : Get observation variable names
- [`load_configuration()`](load_configuration.md) : Load model
  configuration to the aeme object
- [`load_output()`](load_output.md) : Load AEME output to the aeme
  object
- [`lookup_aeme_vars()`](lookup_aeme_vars.md) : View AEME variables
- [`model_controls`](model_controls.md) : Model controls
- [`model_layer_structure`](model_layer_structure.md) : Reference data
  frame for model layer structure.
- [`names(`*`<Aeme>`*`)`](names-aeme-method.md) : Update names Method
- [`o2_at_sat()`](o2_at_sat.md) : Estimate oxygen saturation
  concentration
- [`observations(`*`<Aeme>`*`)`](observations-aeme-method.md) : Access
  observations slot
- [`` `observations<-`( ``*`<Aeme>`*`)`](observations-set-aeme-method.md)
  : Set observations in Aeme object
- [`` `observations<-`() ``](observations-set.md) : Set observations in
  Aeme object
- [`observations()`](observations.md) : Access observations slot
- [`open_nc_safe()`](open_nc_safe.md) : Safely open a NetCDF file
- [`outflows(`*`<Aeme>`*`)`](outflows-aeme-method.md) : Access outflows
  slot
- [`` `outflows<-`( ``*`<Aeme>`*`)`](outflows-set-aeme-method.md) : Set
  outflows in Aeme object
- [`` `outflows<-`() ``](outflows-set.md) : Set outflows in Aeme object
- [`outflows()`](outflows.md) : Access outflows slot
- [`output(`*`<Aeme>`*`)`](output-aeme-method.md) : Access output slot
- [`` `output<-`( ``*`<Aeme>`*`)`](output-set-aeme-method.md) : Set
  output in Aeme object
- [`` `output<-`() ``](output-set.md) : Set output in Aeme object
- [`output()`](output.md) : Access output slot
- [`parameters(`*`<Aeme>`*`)`](parameters-aeme-method.md) : Access
  parameters slot
- [`` `parameters<-`( ``*`<Aeme>`*`)`](parameters-set-aeme-method.md) :
  Set parameters in Aeme object
- [`` `parameters<-`() ``](parameters-set.md) : Set parameters in Aeme
  object
- [`parameters()`](parameters.md) : Access parameters slot
- [`param_colnames()`](param_colnames.md) : Get AEME parameters data
  frame column names
- [`plot(`*`<Aeme>`*`)`](plot-Aeme-method.md) : Update summary Method
- [`plot_flows()`](plot_flows.md) : Plot inflows and/or outflows
- [`plot_fluxes()`](plot_fluxes.md) : Plot fluxes
- [`plot_hyps()`](plot_hyps.md) : Plot hypsograph
- [`plot_met_tile()`](plot_met_tile.md) : Plot a tile plot of
  meteorological data
- [`plot_nit()`](plot_nit.md) : Plot phytoplankton variables
- [`plot_obs()`](plot_obs.md) : Plot observations
- [`plot_output()`](plot_output.md) : Plot AEME output
- [`plot_phs()`](plot_phs.md) : Plot phytoplankton variables
- [`plot_phytos()`](plot_phytos.md) : Plot phytoplankton variables
- [`plot_resid()`](plot_resid.md) : Plot model residuals
- [`plot_ts()`](plot_ts.md) : Plot multi-variable timeseries
- [`plot_var()`](plot_var.md) : Plot AEME variable
- [`plot_wbal()`](plot_wbal.md) : Plot water balance components
- [`plot_wbal_annual()`](plot_wbal_annual.md) : Plot annual water
  balance components
- [`plot_wbal_comp()`](plot_wbal_comp.md) : Plot Water Balance
  Comparison
- [`plot_wbal_summaries()`](plot_wbal_summaries.md) : Plot Water Balance
  Summaries
- [`plot_wlev()`](plot_wlev.md) : Plot lake water level
- [`plot_zoops()`](plot_zoops.md) : Plot phytoplankton variables
- [`precip_status()`](precip_status.md) : Get current precipitation
  status in Aeme object
- [`read_aed_param_csv()`](read_aed_param_csv.md)
  [`write_aed_param_csv()`](read_aed_param_csv.md) : Read and write AED
  parameter CSV files These functions handle reading and writing AED
  parameter CSV files, ensuring that single quotes are properly managed
  in column names and specific columns.
- [`read_aeme_from_files()`](read_aeme_from_files.md) : Read an AEME
  object from files
- [`read_dy_output()`](read_dy_output.md) : Read DYRESM output
- [`read_dy_wlev()`](read_dy_wlev.md) : Read DYRESM water level output
- [`read_glm_output()`](read_glm_output.md) : Read GLM netCDF output
- [`read_glm_wlev()`](read_glm_wlev.md) : Read GLM lake water level
  output
- [`read_gotm_flux_output()`](read_gotm_flux_output.md) : Read GOTM flux
  output
- [`read_gotm_output()`](read_gotm_output.md) : Read GOTM output
- [`read_gotm_profile()`](read_gotm_profile.md) : Read GOTM profile file
- [`read_gotm_wlev()`](read_gotm_wlev.md) : Read GOTM water level output
- [`read_model_config()`](read_model_config.md) : Read in model
  configuration files for a given model and lake directory
- [`read_model_outputs()`](read_model_outputs.md) : Read model outputs
  and format to AEME standard
- [`read_model_wlev()`](read_model_wlev.md) : Read water level from
  model output
- [`read_nml()`](read_nml.md) : read in a GLM simulation \*.nml file
- [`remove_inflow()`](remove_inflow.md) : Remove inflow data from Aeme
  object
- [`remove_outflow()`](remove_outflow.md) : Remove outflow data from
  Aeme object
- [`remove_output()`](remove_output.md) : Remove output from AEME object
- [`remove_param()`](remove_param.md) : Remove parameter(s) from Aeme
  object
- [`reset_wbal_param()`](reset_wbal_param.md) : Reset water balance
  parameters
- [`run_aeme()`](run_aeme.md) : Run aquatic model ensemble
- [`run_dy_cd()`](run_dy_cd.md) [`run_glm_aed()`](run_dy_cd.md)
  [`run_gotm_wet()`](run_dy_cd.md) : Run AEME models
- [`set_aed_sed_const2d()`](set_aed_sed_const2d.md) : Setup AED
  aed_sed_const2d sediment model parameters
- [`set_aed_totals()`](set_aed_totals.md) : Setup AED aed_totals
  parameters
- [`set_glm_aed_models()`](set_glm_aed_models.md) : Set GLM-AED Models
- [`set_gotm_grid()`](set_gotm_grid.md) : Set grid for GOTM model
- [`set_inflows()`](set_inflows.md) : Set inflow data from Aeme object
- [`set_model_controls()`](set_model_controls.md) : Set model controls
  for AEME object
- [`set_nml()`](set_nml.md) : sets values in nml object
- [`set_precip()`](set_precip.md) : Set precipitation as either
  meteorological input or inflow
- [`set_time()`](set_time.md) : Set time parameters for an Aeme object
- [`set_vars_sim()`](set_vars_sim.md) : Set simulation variables in
  model controls
- [`show(`*`<Aeme>`*`)`](show-aeme-method.md) : Print Aeme object to the
  console
- [`summary(`*`<Aeme>`*`)`](summary-aeme-method.md) : Summarise an Aeme
  object
- [`time(`*`<Aeme>`*`)`](time-aeme-method.md) : Access time slot
- [`` `time<-`( ``*`<Aeme>`*`)`](time-set-aeme-method.md) : Set time in
  Aeme object
- [`` `time<-`() ``](time-set.md) : Set time in Aeme object
- [`time()`](time.md) : Access time slot
- [`toggle_models()`](toggle_models.md) : Toggle between model display
  names and codes
- [`update_init()`](update_init.md) : Update initial conditions in AEME
  object based on lake observations.
- [`water_balance(`*`<Aeme>`*`)`](water_balance-aeme-method.md) : Access
  water_balance slot
- [`` `water_balance<-`( ``*`<Aeme>`*`)`](water_balance-set-aeme-method.md)
  : Set water_balance in Aeme object
- [`` `water_balance<-`() ``](water_balance-set.md) : Set water_balance
  in Aeme object
- [`water_balance()`](water_balance.md) : Access water_balance slot
- [`write_aeme_to_files()`](write_aeme_to_files.md) : Write an AEME
  object to files
- [`write_configuration()`](write_configuration.md) : Write model
  configuration from the aeme object
- [`write_nml()`](write_nml.md) : write GLM .nml for a GLM simulation
- [`write_yaml()`](write_yaml.md) : Write a yaml object to file
- [`yaml_to_aeme()`](yaml_to_aeme.md) : Convert aeme.yaml file to list
