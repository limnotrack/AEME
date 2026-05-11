utils::globalVariables(c(

  # Lake/hydrology variables
  "A_t",
  "B",
  "C",
  "CHM_salt",
  "Component",
  "Date",
  "DateTime",
  "Date_dodge",
  "Evaporation",
  "HYD_flow",
  "HYD_outflow",
  "HYD_temp",
  "Inflow",
  "LKE_outflow",
  "LKE_tli3",
  "LKE_tli4",
  
  # Meteorological variables
  "MET_humrel",
  "MET_pprain",
  "MET_ppsnow",
  "MET_prsttn",
  "MET_prvapr",
  "MET_radswd",
  "MET_tmpair",
  "MET_wndspd",
  "MET_wnduvu",
  "MET_wnduvv",
  
  # Model/output variables
  "Model",
  "NIT_tn",
  "Outflow",
  "PHS_tp",
  "PHY_tchla",
  "Precipitation",
  "Q",
  "Qlh",
  "RAD_secchi",
  "Residual",
  "SWR",
  "SWR_diff",
  "SWR_hr",
  
  # Thermal/physical variables
  "T5avg",
  "Tb",
  "Tdiff",
  "Ts",
  "V",
  "VOL",
  
  # Date/time helpers
  "adj_Date",
  "adj_year",
  
  # Atmospheric variables
  "airp",
  "airt",
  "area",
  
  # Bathymetry/depth
  "bathy_extrap",
  "bias",
  "combn",
  "conversion_aed",
  "dV",
  "datetime",
  "deltaV",
  "deps",
  "depth",
  "depth_from",
  "depth_mid",
  "depth_to",
  "derived",
  "derived_from",
  "do_sat",
  
  # Model parameter libraries
  "dy_cd",
  "dy_cd_parameters",
  "elapsed_days",
  "elev",
  "ens",
  "es",
  "est",
  "evap_flux",
  "evap_m3",
  "expected_flux",
  "fdepth",
  "flag",
  "flow",
  "flux_m3",
  "fprop_depth",
  
  # Model-specific parameters
  "glm_aed",
  "glm_aed_parameter_library",
  "glm_aed_parameters",
  "gotm_wet_parameters",
  "group",
  
  # Physical/numerical
  "h",
  "h_inv",
  "hum",
  "hum_vars",
  "hyd_doy",
  "hyd_year",
  "index",
  "inf",
  "infName",
  "inf_default",
  "inflow",
  "initial_wc",

  # Meteorological flag variables
  "is.airmax",
  "is.airmin",
  "is.cldcvr",
  "is.dewmax",
  "is.dewmin",
  "is.humrel",
  "is.ppsnow",
  "is.prmslp",
  "is.prvapr",
  "is.radlwd",
  "is.tmpdew",
  "is.wnduvu",
  
  # Naming/labelling
  "key_naming",
  "kmeans",
  "label",
  "lake_id",
  "level",
  "level_resid",
  "loc",
  "lower",
  "lvl_adj",
  "lvl_obs",
  "lvl_sim",
  "lvl_start",
  "lyr_thk",
  
  # Performance metrics
  "mae",
  "mb_residual",
  "mid_next",
  "mid_prev",
  "model",
  "model_controls",
  "model_layer_structure",
  
  # Layer counts
  "n_above",
  "n_below",
  "n_na",
  
  # Base R functions called without namespace prefix
  "na.omit",
  
  # Variable naming
  "name",
  "name_parse",
  "name_text",
  "net",
  "net_flux",
  "new",
  
  # Performance metrics
  "nse",
  "obs",
  "obs_O",
  "obs_na",
  "obs_v",
  "outflow",
  "outflow_total",
  
  # Percentiles
  "p05",
  "p95",
  "p_name",
  "period_date",
  "precip",
  "precip_m",
  
  # Base R functions called without namespace prefix
  "predict",
  
  # Physical/depth
  "prop_depth",
  "prvapr",
  
  # Performance metrics
  "r2",
  "rain",
  
  # Base R functions called without namespace prefix
  "read.table",
  
  # Residuals/simulation
  "resid",
  "residual",
  "residuals",
  "season",
  "sim",
  "sim_na",
  "simulate",
  
  # Hydrology
  "spill_outflow",
  "sst",
  "strat",
  "strat_days",
  "surf",
  "temperature",
  "type",
  "upper",
  "value",
  "var",
  "var_aeme",
  "var_sim",
  "variable",

  # Year/zone classification
  "year_bin",
  "year_class",
  "year_count",
  "z",
  "zi",
  "zone"
))
