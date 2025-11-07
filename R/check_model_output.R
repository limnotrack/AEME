#' Check model output
#'
#' @inheritParams build_aeme
#' @return Invisibly TRUE if model output passes checks; otherwise aborts
#' @export
check_model_output <- function(aeme, model, path) {
  model <- check_model(model = model)
  path  <- check_path(path = path, must_exist = TRUE)
  aeme_time <- time(aeme)
  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  
  out_file <- switch(
    model,
    "dy_cd"    = file.path(lake_dir, model, "DYsim.nc"),
    "glm_aed"  = file.path(lake_dir, model, "output", "output.nc"),
    "gotm_wet" = file.path(lake_dir, model, "output", "output.nc")
  )
  
  # Open NetCDF safely
  nc <- open_nc_safe(out_file, model)
  
  # Model-specific validation
  switch(model,
         "dy_cd"    = check_dyresm_output(nc, out_file, aeme_time),
         "gotm_wet" = check_gotm_output(nc, out_file),
         "glm_aed"  = check_glm_output(nc, out_file)
  )
  
  cli_inform_safe("{.val {model}} output file {.file {out_file}} is valid.")
  invisible(TRUE)
}


#' Safely open a NetCDF file
#'
#' @param file path to NetCDF file
#' @param model name of the model (for error messages)
#' @return ncdf4 object
#' @noRd
open_nc_safe <- function(file, model) {
  if (!file.exists(file)) {
    cli::cli_abort("{.val {model}} output file not found: {.file {file}}",
                   class = "aeme_error_model_output_missing")
  }
  
  nc <- tryCatch(
    ncdf4::nc_open(file, return_on_error = TRUE),
    error = function(e) {
      cli::cli_abort("{.val {model}} output file cannot be opened: {.file {file}}",
                     class = "aeme_error_model_output_corrupt")
    }
  )
  
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  nc
}

#' Check DYRESM-CAEDYM output for NA dates
#' 
#' @param nc ncdf4 object of DYRESM-CAEDYM output file
#' @param out_file path to DYRESM-CAEDYM output file
#' @param aeme_time list with start and end POSIXct times of simulation
#' @return Invisibly TRUE if output passes checks; otherwise aborts
#' @noRd
check_dyresm_output <- function(nc, out_file, aeme_time) {
  dates <- ncdf4::ncvar_get(nc, "dyresmTime")
  dates[dates > 9e36] <- NA
  if (any(is.na(dates))) {
    last_date <- dates[which.max(is.na(dates)) - 1]
    last_date <- as.POSIXct((last_date - 2415018.5) * 86400, origin = "1899-12-30")
    msg <- if (length(last_date) == 0) {
      "DYRESM-CAEDYM crashed during initialization; no output available."
    } else if (last_date < aeme_time$start) {
      "DYRESM-CAEDYM crashed during spin-up; no output available for simulation period."
    } else {
      paste0("DYRESM-CAEDYM output file {.file {out_file}} contains NA dates.")
    }
    cli::cli_abort(msg, class = "aeme_error_dyresm_output")
  }
}

#' Check GOTM-WET output for depth issues
#' 
#' @param nc ncdf4 object of GOTM-WET output file
#' @param out_file path to GOTM-WET output file
#' @return Invisibly TRUE if output passes checks; otherwise aborts
#' @noRd
check_gotm_output <- function(nc, out_file) {
  zi <- ncdf4::ncvar_get(nc, "zi")
  if (is.null(dim(zi))) {
    cli::cli_abort("GOTM-WET output file {.file {out_file}} has invalid 'zi' variable.",
                   class = "aeme_error_gotm_output")
  }
  init_z <- round(min(zi[, 1]) - max(zi[, 1]), 2)
  if (any(zi < init_z)) {
    cli::cli_abort(
      c(
        "!" = "GOTM-WET output file {.file {out_file}}: Model depth goes below initial depth.",
        "i" = "Possible cause: in/outflows. Adjust scaling factors and re-run the model."
      ),
      class = "aeme_error_gotm_depth"
    )
  }
}

#' Placeholder for GLM-AED output checks
#' @param nc ncdf4 object of GLM-AED output file
#' @param out_file path to GLM-AED output file
#' @return Invisibly TRUE if output passes checks; otherwise aborts
#' @noRd
check_glm_output <- function(nc, out_file) {
  # Currently no additional checks, but this is the placeholder
  # for future GLM-AED output validations.
  # If the nc object has errors, open_nc_safe() will already abort.
  cli_inform_safe("GLM-AED output file {.file {out_file}} passed initial checks.")
}


