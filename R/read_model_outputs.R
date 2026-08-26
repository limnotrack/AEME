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
#' @param load_all logical; for `model = "glm_aed"`, also load every other
#' variable present in the netCDF output beyond the declared `vars_sim` set
#' -- see `?read_glm_output`. Ignored for other models. Defaults to TRUE.
#' @param use_dat logical; for the Simstrat models only, read Simstrat's own
#' `<var>_out.dat` text output via \code{\link{read_simstrat_dat}} instead of
#' the consolidated `output.nc`. Every other argument means the same thing
#' either way, so this only changes where the numbers are read from. `TRUE`
#' is the faster path -- it skips the netCDF entirely, and with
#' `load_all = FALSE` reads only the files the requested `vars_sim` need,
#' which is what a calibration wants. Defaults to `NULL`: read `output.nc`
#' when there is one, and fall back to the text output when there is not
#' (a run whose output was never converted, or converted with
#' \code{\link{write_simstrat_nc}}`(remove_dat = FALSE)` and the netCDF since
#' removed). Ignored when `nc` is supplied.
#'
#' @importFrom ncdf4 nc_open nc_close ncvar_get ncatt_get
#' @importFrom withr local_locale local_timezone
#' @importFrom dplyr filter pull mutate arrange desc
#' @returns List of model outputs in AEME standard format
#' @export

read_model_outputs <- function(nc = NULL, lake_dir, model, vars_sim = NULL,
                               depths = NULL, dates = NULL, date_index = NULL,
                               incl_fluxes = TRUE, output_hour = 0,
                               phyto_pars = NULL, load_all = TRUE,
                               use_dat = NULL) {

  # Set timezone
  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")

  model <- check_model(model)
  if (length(model) != 1) {
    cli::cli_abort("Please supply a single model name.")
  }

  # ---- netCDF or Simstrat's own text output? ----
  is_simstrat <- model %in% c("simstrat_aed2", "simstrat_aed")
  if (isTRUE(use_dat) && !is_simstrat) {
    cli::cli_abort(c(
      "x" = "{.arg use_dat} only applies to the Simstrat models, not {.val {model}}.",
      "i" = "Only Simstrat writes its output as text alongside a netCDF."
    ))
  }
  auto_dat <- is.null(use_dat) && is_simstrat && is.null(nc)
  use_dat <- isTRUE(use_dat) && is.null(nc)

  nc_files <- NULL
  if (!use_dat && is.null(nc)) {
    lake_dir <- check_path(lake_dir, must_exist = TRUE)
    # Read in model netCDF file
    nc_files <- if (auto_dat) {
      # The netCDF may legitimately be absent here -- that is what the
      # fall-back below is for -- so a failure to resolve it is not yet an
      # error.
      tryCatch(get_model_outfile(model = model, path = lake_dir)[[model]],
               error = function(e) character(0))
    } else {
      get_model_outfile(model = model, path = lake_dir)[[model]]
    }
    if (auto_dat && (length(nc_files) == 0 || !all(file.exists(nc_files)))) {
      use_dat <- .simstrat_dat_available(lake_dir = lake_dir, model = model)
      if (!use_dat) {
        # Neither form of output is there: let the netCDF path report it,
        # so the error is the one callers already handle.
        nc_files <- get_model_outfile(model = model, path = lake_dir)[[model]]
      }
    }
  }

  if (use_dat) {
    # Simstrat's raw text output. read_simstrat_dat() takes vars_sim/depths/
    # dates/date_index/incl_fluxes/load_all with the same meaning as the
    # netCDF readers below, and returns the same output list, so nothing
    # downstream needs to know which path was taken.
    lake_dir <- check_path(lake_dir, must_exist = TRUE)
    hyps <- read_model_hypsograph(model = model, lake_dir = lake_dir)
    out_list <- read_simstrat_dat(sim_folder = file.path(lake_dir, model),
                                  vars_sim = vars_sim, depths = depths,
                                  dates = dates, date_index = date_index,
                                  incl_fluxes = incl_fluxes,
                                  load_all = load_all, model = model)
    if (is_model_error(out_list)) return(out_list)
    return(.finalise_model_output(out_list = out_list, hyps = hyps,
                                  vars_sim = vars_sim, model = model))
  }

  if (is.null(nc)) {
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
                                                  phyto_pars = phyto_pars,
                                                  load_all = load_all),
                     "dy_cd"    = read_dy_output(nc, vars_sim, depths = depths,
                                                 incl_fluxes = incl_fluxes,
                                                 date_index = date_index),
                     "simstrat_aed2" = read_simstrat_output(nc, vars_sim,
                                                            depths = depths,
                                                            incl_fluxes = incl_fluxes,
                                                            date_index = date_index,
                                                            model = "simstrat_aed2"),
                     "simstrat_aed" = read_simstrat_output(nc, vars_sim,
                                                           depths = depths,
                                                           incl_fluxes = incl_fluxes,
                                                           date_index = date_index,
                                                           model = "simstrat_aed")
  )
  
  if (model == "gotm_wet" & !incl_fluxes & read_gotm_daily) {
    add_vars <- read_gotm_output(file = nc_files["output_daily"], 
                                 incl_fluxes = TRUE, date_index = date_index)
    # Add missing vars to out_list
    missing_vars <- names(add_vars)[!names(add_vars) %in% names(out_list)]
    add_vars <- add_vars[missing_vars]
    
    out_list <- c(out_list, add_vars)
  }
  
  return(.finalise_model_output(out_list = out_list, hyps = hyps,
                                vars_sim = vars_sim, model = model))
}

#' Add derived variables, flatten 1-D variables, and tag the output list
#'
#' Steps 4 and 5 of [read_model_outputs()], shared by every way of getting
#' the output list -- the netCDF readers and, for Simstrat, the raw-text
#' [read_simstrat_dat()] -- so the two cannot drift apart.
#'
#' @param out_list list; the model output list from a reader.
#' @param hyps dataframe; the model hypsograph, for the derived variables.
#' @param vars_sim character; requested AEME variables, which decide the
#'   derived variables to add.
#' @param model character; the model that produced the output.
#' @return `out_list`, classed by `.new_aeme_output()`.
#' @noRd
.finalise_model_output <- function(out_list, hyps, vars_sim, model) {

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

  return(.new_aeme_output(out_list, model = model))
}

#' Has a Simstrat run left its `<var>_out.dat` text output behind?
#'
#' @param lake_dir character; the lake directory.
#' @param model character; `"simstrat_aed2"` or `"simstrat_aed"`.
#' @return logical; `FALSE` if the simulation directory, its `simstrat.par`,
#'   or its output files are missing.
#' @noRd
.simstrat_dat_available <- function(lake_dir, model) {
  sim_folder <- file.path(lake_dir, model)
  if (!dir.exists(sim_folder)) return(FALSE)
  info <- tryCatch(.simstrat_par_paths(sim_folder = sim_folder),
                   error = function(e) NULL)
  if (is.null(info) || !dir.exists(info$out_dir)) return(FALSE)
  length(list.files(info$out_dir, pattern = "_out\\.dat$")) > 0
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
    dplyr::filter(var_aeme %in% vars_sim & !derived) |> 
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
  } else if (model %in% c("simstrat_aed2", "simstrat_aed")) {
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

  model <- check_model(model)
  lake_dir <- check_path(lake_dir, must_exist = TRUE)
  cfg_files <- get_model_config_files(model = model,
                                      path = lake_dir)[[model]]

  if (missing(file)) {
    file <- switch(model,
                   "gotm_wet" = "gotm",
                   "glm_aed"  = find_glm_nml_key(names(cfg_files)),
                   "dy_cd"    = "stg",
                   "simstrat_aed2" = "simstrat",
                   "simstrat_aed" = "simstrat")
  }
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
  } else if (model %in% c("simstrat_aed2", "simstrat_aed")) {
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

  } else if (model %in% c("simstrat_aed2", "simstrat_aed")) {
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


