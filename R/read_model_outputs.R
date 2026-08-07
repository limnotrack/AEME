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
#' @param phyto_pars Dataframe of phytoplankton parameters for GLM-AED model. 
#' See `?read_glm_output` for details. Defaults to NULL.
#'
#' @importFrom ncdf4 nc_open nc_close ncvar_get ncatt_get
#' @importFrom withr local_locale local_timezone
#' @importFrom dplyr filter pull mutate arrange desc
#' @returns List of model outputs in AEME standard format
#' @export

read_model_outputs <- function(nc = NULL, lake_dir, model, vars_sim = NULL, 
                               depths = NULL, dates = NULL, date_index = NULL,
                               incl_fluxes = TRUE, output_hour = 0, 
                               phyto_pars = NULL) {
  
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
    nc_files <- get_model_outfile(model = model, path = lake_dir)[[model]]
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
  hyps <- read_model_hypsograph(model = model, lake_dir = lake_dir)
  
  if (is.null(date_index)) {
    # ---- 1. extract time info for this model
    time_info <- extract_model_time(nc = nc, model = model)
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
                                                  date_index = date_index,
                                                  phyto_pars = phyto_pars),
                     "dy_cd"    = read_dy_output(nc, vars_sim, depths = depths,
                                                 incl_fluxes = incl_fluxes,
                                                 date_index = date_index),
                     "simstrat_aed2" = read_simstrat_output(nc, vars_sim,
                                                            depths = depths,
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
    dplyr::filter(var_aeme %in% vars_sim & derived) |> 
    dplyr::pull(var_aeme)
  if (length(deriv_vars) > 0) {
    out_list <- add_deriv_output(out_list = out_list, hyps = hyps, 
                                 vars_sim = deriv_vars)
  }
  
  # ---- 5. convert all 1 dimension variables to vectors
  vars_1d <- c("LKE_lvlwtr", "HYD_surft", 
               "LKE_Qe", "LKE_Qh", "LKE_Qlw", "LKE_Qsw", 
               "LKE_inflow", "LKE_outflow", "LKE_overflow", "LKE_outftot",
               "LKE_A0", "LKE_V",
               "LKE_evpflx", "LKE_evpvol", "LKE_pcpvol",
               # Derived vars
               "HYD_thmcln", "HYD_strat", "HYD_ctrbuy", "HYD_epidep", 
               "HYD_hypdep", "HYD_schstb", "CHM_oxycln", "CHM_oxyepi", "CHM_oxyhyp", 
               "CHM_oxymet", "CHM_oxymom", "CHM_oxynal", "LKE_tlic", "LKE_tlin", 
               "LKE_tlip", "LKE_tlise", "LKE_tli3", "LKE_tli4"
               )
  vars_1d_in <- vars_1d[vars_1d %in% names(out_list)]
  if (length(vars_1d_in) > 0) {
    # Convert to vector
    for (var in vars_1d_in) {
      out_list[[var]] <- as.vector(out_list[[var]])
    }
  }

  return(out_list)
}


#' Get model variable names and conversion factors for AEME variables
#' @param vars_sim Variables to extract in the AEME format e.g. "HYD_temp"
#' @param model Model name. One of "gotm_wet", "glm_aed", or "dy_cd".
#' @param as_vector Logical; if TRUE returns a named vector of model variable
#'   names, if FALSE returns a dataframe with conversion factors. Default FALSE.
#' @return Dataframe of model variable names and conversion factors, or a named
#'   vector of model variable names if \code{as_vector = TRUE}.
#' @keywords internal
#' @noRd
get_model_vars <- function(vars_sim, model, as_vector = FALSE) {
  data("key_naming", package = "AEME", envir = environment())
  model_vars <- key_naming |> 
    dplyr::filter(var_aeme %in% vars_sim & !derived & var_aeme != "LKE_lvlwtr") |> 
    dplyr::select(var_aeme, dplyr::sym(model), conversion_aed)
  
  # If any variables are not in key_naming add them as separate rows
  if (any(!vars_sim %in% model_vars$var_aeme)) {
    missing_vars <- vars_sim[!vars_sim %in% model_vars$var_aeme]
    missing_df <- data.frame(var_aeme = missing_vars,
                             conversion_aed = 1)
    missing_df[[model]] <- missing_vars
    model_vars <- dplyr::bind_rows(model_vars, missing_df) |> 
      dplyr::arrange(match(var_aeme, vars_sim))
  }
  # Check if any variables in model column are ""
  missing_vars <- model_vars |> 
    dplyr::filter(!!dplyr::sym(model) == "") |> 
    dplyr::pull(var_aeme)
  if (length(missing_vars) > 0) {
    msg <- paste0("The following variables are not available in model 
                   ", model, ": ", paste0(missing_vars, collapse = ", "))
    cli_inform_safe(c("!" = msg))
    model_vars <- model_vars |>
      dplyr::filter(!var_aeme %in% missing_vars)
  }  
  if ("dy_cd" %in% model) {
    model_vars <- model_vars |>
      dplyr::mutate(dy_cd = paste0("dyresm", dy_cd, "_Var"))
  }
  
  if (as_vector) {
    return(setNames(model_vars[[model]], model_vars$var_aeme))
  }
  return(model_vars)
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
  } else if (model == "simstrat_aed2") {
    hyps <- read_model_hypsograph(model = model, lake_dir = lake_dir)
    depth <- max(hyps$elev) - min(hyps$elev)
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
                   "dy_cd"    = "stg",
                   "simstrat_aed2" = "simstrat")
  }
  model <- check_model(model)
  lake_dir <- check_path(lake_dir, must_exist = TRUE)
  cfg_files <- get_model_config_files(model = model,
                                      path = lake_dir)[[model]]
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
  } else if (model == "simstrat_aed2") {
    cfg <- jsonlite::fromJSON(cfg_file, simplifyVector = FALSE)
  }
  return(cfg)
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

  } else if (model == "simstrat_aed2") {
    units_prefix <- "seconds since "
    t <- ncdf4::ncvar_get(nc, "time")
    origin <- gsub(units_prefix, "", ncdf4::ncatt_get(nc, "time", "units")$value)
    dt <- as.POSIXct(t, origin = origin, tz = "UTC")

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

#' Create an empty model output structure with error reason
#' @param reason Reason for empty model output
#' @return Structure with class "model_output_error"
#' @keywords internal
#' @noRd
empty_model_output <- function(reason) {
  structure(
    list(ok = FALSE, reason = reason),
    class = "model_output_error"
  )
}

#' Check if object is a model output error
#' @param x Object to check
#' @return Logical indicating if object is a model output error
#' @export
is_model_error <- function(x) inherits(x, "model_output_error")


has_error      <- function(x) !isTRUE(x$ok)


