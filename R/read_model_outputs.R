#' Read model outputs and format to AEME standard
#'
#' @param nc Open netCDF object. If NULL, will open netCDF from lake_dir. This
#' is useful when reading multiple variables from the same file to avoid
#' reopening the file multiple times. Defaults to NULL.
#' @param lake_dir Directory of lake model outputs
#' @param model Model name. One of "gotm_wet", "glm_aed", or "dy_cd".
#' @param vars_sim Variables to extract in the AEME format e.g. "HYD_temp"
#' @param depths Depths to extract. If NULL, extract all model layer depths. 
#' Defaults to NULL.
#' @param dates Dates to extract. If NULL, extract all dates. Defaults to NULL.
#' @param date_index Date index to extract. If NULL, extract all dates. Defaults
#' to NULL.
#' @param incl_fluxes Logical indicating whether to include flux variables.
#' Defaults to TRUE.
#' @param output_hour Hour of the day to extract (0-23). Defaults to 0.
#'
#' @importFrom ncdf4 nc_open nc_close ncvar_get ncatt_get
#' @importFrom withr local_locale local_timezone
#' @importFrom dplyr filter pull mutate arrange desc
#' @returns List of model outputs in AEME standard format
#' @export

read_model_outputs <- function(nc = NULL, lake_dir, model, vars_sim = NULL, 
                               depths = NULL, dates = NULL, date_index = NULL,
                               incl_fluxes = TRUE, output_hour = 0) {
  
  # Set timezone
  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")
  
  
  model <- check_model(model)
  if (length(model) != 1) {
    cli::cli_abort("Please supply a single model name.")
  }
  if (is.null(nc)) {
    lake_dir <- check_path(lake_dir, must_exist = TRUE)
    # Read in model netCDF file
    nc_files <- get_model_outfile(model = model, lake_dir = lake_dir)[[model]]
    if (model == "gotm_wet") {
      nc_file <- nc_files["output"]  
      incl_fluxes <- ifelse("output_daily" %in% names(nc_files), FALSE, TRUE)
      read_gotm_daily <- !incl_fluxes
    } else {
      nc_file <- nc_files
      read_gotm_daily <- FALSE
    }
    nc <- open_nc_safe(file = nc_file, model = model)
    on.exit(ncdf4::nc_close(nc), add = TRUE)
  } else {
    read_gotm_daily <- FALSE
  }

  # Load model hypsograph
  hyps <- load_model_hypsograph(model = model, lake_dir = lake_dir)
  
  if (is.null(date_index)) {
    # ---- 1. extract time info for this model
    time_info <- extract_model_time(nc, model)
    dt <- time_info$datetime
    date_index <- seq_along(dt)
    
    # ---- 2. subset by dates if supplied
    if (!is.null(dates)) {
      dates <- as.Date(dates)
      date_index <- which(as.Date(dt) %in% dates)
    }
  }
  
  if (!length(date_index))
    cli::cli_abort("No model output found for requested dates.")
  
  # ---- 3. dispatch to model-specific extractor
  out_list <- switch(model,
                     "gotm_wet" = read_gotm_output(nc, vars_sim, 
                                                   depths = depths,
                                                   incl_fluxes = incl_fluxes, 
                                                   date_index = date_index),
                     "glm_aed"  = read_glm_output(nc, vars_sim, depths = depths,
                                                  incl_fluxes = incl_fluxes, 
                                                  date_index = date_index),
                     "dy_cd"    = read_dy_output(nc, vars_sim, depths = depths,
                                                 incl_fluxes = incl_fluxes, 
                                                 date_index = date_index)
  )
  
  if (model == "gotm_wet" & !incl_fluxes & read_gotm_daily) {
    add_vars <- read_gotm_output(file = nc_files["output_daily"], 
                                 incl_fluxes = TRUE, date_index = date_index)
    # Add missing vars to out_list
    missing_vars <- names(add_vars)[!names(add_vars) %in% names(out_list)]
    add_vars <- add_vars[missing_vars]
    
    out_list <- c(out_list, add_vars)
  }
  
  # ---- 4. add derivative outputs
  data("key_naming", package = "AEME", envir = environment())
  deriv_vars <- key_naming |> 
    dplyr::filter(name %in% vars_sim & derived) |> 
    dplyr::pull(name)
  if (length(deriv_vars) > 0) {
    out_list <- add_deriv_output(out_list = out_list, hyps = hyps, 
                                 vars_sim = deriv_vars)
  }
  
  # ---- 5. convert all 1 dimension variables to vectors
  vars_1d <- c("LKE_lvlwtr", "HYD_surft", "LKE_Qe", "LKE_Qh", "LKE_Qlw", 
               "LKE_Qsw", "LKE_inflow", "LKE_outflow", "LKE_A0", "LKE_V",
               "LKE_evpflx", "LKE_evpvol", "LKE_pcpvol",
               # Derived vars
               "HYD_thmcln", "HYD_strat", "HYD_ctrbuy", "HYD_epidep", 
               "HYD_hypdep", "HYD_schstb", "CHM_oxycln", "CHM_oxyepi", "CHM_oxyhyp", 
               "CHM_oxymet", "CHM_oxymom", "CHM_oxynal", "LKE_tlic", "LKE_tlin", 
               "LKE_tlip", "LKE_tlise", "LKE_tli3", "LKE_tli4"
               )
  for (var in vars_1d) {
    if (var %in% names(out_list)) {
      out_list[[var]] <- as.vector(out_list[[var]])
    }
  }

  return(out_list)
}


#' Get model variable names and conversion factors for AEME variables
#' @param vars_sim Variables to extract in the AEME format e.g. "HYD_temp"
#' @param model Model name. One of "gotm_wet", "glm_aed", or "dy_cd".
#' @return Dataframe of model variable names and conversion factors
#' @keywords internal
#' @noRd
get_model_vars <- function(vars_sim, model) {
  data("key_naming", package = "AEME", envir = environment())
  model_vars <- key_naming |> 
    dplyr::filter(name %in% vars_sim & !derived & name != "LKE_lvlwtr") |> 
    dplyr::select(name, dplyr::sym(model), conversion_aed)
  if ("dy_cd" %in% model) {
    model_vars <- model_vars |>
      dplyr::mutate(dy_cd = paste0("dyresm", dy_cd, "_Var"))
  }
  # Check if any variables in model column are ""
  missing_vars <- model_vars |> 
    dplyr::filter(!!dplyr::sym(model) == "") |> 
    dplyr::pull(name)
  if (length(missing_vars) > 0) {
    cli::cli_abort("The following variables are not available in model 
                   {.val {model}}: {.val {missing_vars}}")
  }
  
  return(model_vars)
}

#' Get model variable names and conversion factors for AEME variables as named 
#' vector
#' @param vars_sim Variables to extract in the AEME format e.g. "HYD_temp"
#' @param model Model name. One of "gotm_wet", "glm_aed", or "dy_cd".
#' @return Named vector of model variable names
#' @keywords internal
#' @noRd
format_model_vars_vec <- function(vars_sim, model) {
  data("key_naming", package = "AEME", envir = environment())
  model_vars <- key_naming |> 
    dplyr::filter(name %in% vars_sim & !derived & name != "LKE_lvlwtr") |> 
    dplyr::select(name, dplyr::sym(model), conversion_aed)
  if ("dy_cd" %in% model) {
    model_vars <- model_vars |>
      dplyr::mutate(dy_cd = paste0("dyresm", dy_cd, "_Var"))
  }
  # Check if any variables in model column are ""
  missing_vars <- model_vars |> 
    dplyr::filter(!!dplyr::sym(model) == "") |> 
    dplyr::pull(name)
  if (length(missing_vars) > 0) {
    cli::cli_abort("The following variables are not available in model 
                   {.val {model}}: {.val {missing_vars}}")
  }
  
  model_vars_vec <- setNames(model_vars[[model]], model_vars$name)
  return(model_vars_vec)
}

# ' Extract model layer depths up to lake depth
#' @param lake_dir Directory of lake model outputs
#' @param model Model name. One of "gotm_wet", "glm_aed", or "dy_cd".
#' @return Vector of model layer depths
#' @keywords internal
#' @noRd
extract_model_depths <- function(lake_dir, model) {
  lake_depth <- extract_model_depth(model = model, lake_dir = lake_dir)
  data("model_layer_structure", package = "AEME", envir = environment())
  depths <- model_layer_structure |> 
    dplyr::filter(z < lake_depth) |> 
    dplyr::pull(z)
  depths <- c(0, depths)
  return(depths)
}

#' Extract model lake depth from configuration
#' @param lake_dir Directory of lake model outputs
#' @param model Model name. One of "gotm_wet", "glm_aed", or "dy_cd".
#' @return Lake depth in meters
#' @keywords internal
#' @noRd
extract_model_depth <- function(model, lake_dir) {
  
  cfg <- load_model_config(model = model, lake_dir = lake_dir)
  if (model == "gotm_wet") {
    depth <- cfg$location$depth
  } else if (model == "glm_aed") {
    depth <- cfg$init_profiles$lake_depth
  } else if (model == "dy_cd") {
    depth <- cfg$lake_depth_m
  }
  return(depth)
}

#' Load model configuration file
#' @param lake_dir Directory of lake model outputs
#' @param model Model name. One of "gotm_wet", "glm_aed", or "dy_cd".
#' @param file Configuration file to load. If missing, defaults to main config file
#' for each model.
#' @return Model configuration as a list or character vector
#' @keywords internal
#' @noRd
load_model_config <- function(model, lake_dir, file) {
  
  if (missing(file)) {
    file <- switch(model,
                   "gotm_wet" = "gotm",
                   "glm_aed"  = "glm3",
                   "dy_cd"    = "stg")
  }
  model <- check_model(model)
  lake_dir <- check_path(lake_dir, must_exist = TRUE)
  cfg_files <- get_model_config_files(model = model, 
                                      lake_dir = lake_dir)[[model]]
  if (file %in% names(cfg_files)) {
    cfg_file <- cfg_files[[file]]
  } else {
    cli::cli_abort("Configuration file {.val {file}} not found for model {.val {model}} in directory {.val {lake_dir}}.")
  }
  if (model == "gotm_wet") {
    cfg <- yaml::read_yaml(cfg_file)
  } else if (model == "glm_aed") {
    cfg <- read_nml(cfg_file)
  } else if (model == "dy_cd") {
    cfg <- readLines(cfg_file)
  }
  return(cfg)
}

#' Load model hypsograph from configuration
#' @param lake_dir Directory of lake model outputs
#' @param model Model name. One of "gotm_wet", "glm_aed", or "dy_cd".
#' @return Dataframe of hypsograph with columns elev, area, and depth
#' @keywords internal
#' @noRd
load_model_hypsograph <- function(model, lake_dir) {
  model <- check_model(model)
  lake_dir <- check_path(lake_dir, must_exist = TRUE)
  cfg <- load_model_config(model = model, lake_dir = lake_dir)
  if (model == "gotm_wet") {
    hyps_filename <- cfg$location$hypsograph
    hyps_file <- file.path(lake_dir, "gotm_wet", hyps_filename)
    hyps <- read_gotm_hyps(file = hyps_file) |> 
      dplyr::mutate(elev = depth)
  } else if (model == "glm_aed") {
    init_depth <- cfg$init_profiles$lake_depth + cfg$morphometry$base_elev
    hyps <- data.frame(elev = cfg$morphometry$H, area = cfg$morphometry$A) |> 
      dplyr::mutate(depth = elev - init_depth) |> 
      dplyr::arrange(dplyr::desc(elev))
  } else if (model == "dy_cd") {
    stg_file <- get_model_config_files(model = model, 
                                       lake_dir = lake_dir)[[model]]["stg"]
    stg <- read_dy_stg(file = stg_file)
    hyps <- stg$bathymetry |> 
      dplyr::mutate(depth = elev - stg$surface_elev) |> 
      dplyr::arrange(dplyr::desc(elev))
  }
  return(hyps)
}

#' Extract model time information
#' @param nc Open netCDF object
#' @param model Model name. One of "gotm_wet", "glm_aed", or "dy_cd".
#' @return List with datetime (POSIXct) and dates (Date)
#' @keywords internal
#' @noRd
extract_model_time <- function(nc, model) {
  
  if (model == "gotm_wet") {
    units_prefix <- "seconds since "
    t <- ncdf4::ncvar_get(nc, "time")
    origin <- gsub(units_prefix, "", ncdf4::ncatt_get(nc, "time", "units")$value)
    dt <- as.POSIXct(t + as.POSIXct(origin), tz = "UTC")
    
  } else if (model == "glm_aed") {
    units_prefix <- "hours since "
    t <- ncdf4::ncvar_get(nc, "time")
    origin <- gsub(units_prefix, "", ncdf4::ncatt_get(nc, "time", "units")$value)
    dt <- as.POSIXct(t * 3600 + as.POSIXct(origin), tz = "UTC")
    
  } else if (model == "dy_cd") {
    dt <- as.POSIXct((ncdf4::ncvar_get(nc, "dyresmTime") - 2415018.5) *
                       86400, origin = "1899-12-30")
    
  }
  
  list(
    datetime = dt,
    dates = as.Date(dt)
  )
}


#' Interpolate variable from model layer midpoints to standard depths
#' @param var Matrix of variable values (layers x time)
#' @param midpoints Matrix of model layer midpoints (layers x time)
#' @param out_depths Matrix of output depths (standard depths x time)
#' @param digits Number of decimal places to round output
#' @return Matrix of interpolated variable values (standard depths x time)
#' @keywords internal
#' @noRd
interp_static_grid <- function(var, midpoints, out_depths, digits = 4) {
  out <- sapply(seq_len(ncol(var)), function(t)
    if (sum(!is.na(midpoints[, t])) < 2 | sum(!is.na(var[, t])) < 2) {
      rep(NA, nrow(out_depths))
    } else{
      approx(midpoints[, t], var[, t], xout = out_depths[, t], rule = 2)$y
    }
  )
  out <- round(out, digits)
  return(out)
}
