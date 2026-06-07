#' Check and convert time input to POSIXct
#'
#' @param x character, Date, or POSIXt object representing time
#' @param tz character; timezone, default is "UTC"
#' 
#' @importFrom cli cli_abort
#'
#' @returns POSIXct object
#' @noRd
check_time_format <- function(x, tz = "UTC") {
  if (inherits(x, "Date")) return(as.POSIXct(x, tz = tz))
  if (inherits(x, "POSIXt")) return(as.POSIXct(x, tz = tz))
  
  if (is.character(x)) {
    formats <- c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d")
    for (fmt in formats) {
      parsed <- as.POSIXct(x, format = fmt, tz = tz)
      if (!any(is.na(parsed))) return(parsed)
    }
    cli::cli_abort(
      c(
        "!" = "Invalid time format detected.",
        "x" = "Input must be in one of: {.val 'YYYY-mm-dd HH:MM:SS'}, {.val 'YYYY-mm-dd HH:MM'}, {.val 'YYYY-mm-dd'}.",
        "i" = "Alternatively, provide a {.cls Date} or {.cls POSIXt} object."
      ),
      class = "aeme_error_time_format"
    )
  }

  cli::cli_abort(
    c(
      "!" = "{.arg x} must be a {.cls character}, {.cls Date}, or {.cls POSIXt} object.",
      "x" = "You supplied a {.cls {class(x)[1]}}."
    ),
    class = "aeme_error_time_type"
  )
}


#' Abort if object is not a data frame
#'
#' Utility function to check that an object is a data frame or tibble.
#'
#' @param df Object to check.
#' @param name Optional name of the object (for informative messages).
#' @param class_suffix Optional string appended to the error class.
#'
#' @returns Invisibly returns the object if it is a data frame; otherwise aborts.
#' @noRd
abort_if_not_dataframe <- function(df, name = NULL, class_suffix = NULL) {
  if (!is.data.frame(df)) {
    cli::cli_abort(
      c(
        "!" = "{.arg {name %||% 'object'}} must be a data frame, not {.cls {class(df)[1]}}."
      ),
      class = c("aeme_error_type", paste0("aeme_error_", class_suffix %||% name))
    )
  }
  
  invisible(df)
}

#' Abort if required columns are missing
#'
#' Utility function to check for required columns in a data frame or tibble.
#'
#' @param df A data frame or tibble.
#' @param required_cols Character vector of required column names.
#' @param name Optional name of the data frame for informative messages (e.g. "met", "hypsograph").
#' @param class_suffix Optional string appended to the error class (default: name of data frame if provided).
#'
#' @returns Invisibly returns the data frame if valid; otherwise aborts with a `cli_abort` error.
#' @noRd
abort_if_missing_cols <- function(df, required_cols, name = NULL, class_suffix = NULL) {
  stopifnot(is.character(required_cols))
  
  missing_cols <- setdiff(required_cols, colnames(df))
  
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      c(
        "!" = "Missing required columns in {.arg {name %||% 'data frame'}}.",
        "x" = "Missing: {paste(missing_cols, collapse = ', ')}",
        "i" = "Expected: {paste(required_cols, collapse = ', ')}"
      ),
      class = c("aeme_error_missing_cols", paste0("aeme_error_", class_suffix %||% name))
    )
  }
  
  invisible(df)
}

#' Check hypsograph data frame
#'
#' @param hypsograph data frame with columns "depth", "area" and "elev". Depth should be
#' monotonic decreasing and area should be monotonic increasing.
#' If NULL, the function will check for hypsograph in the input slot of the Aeme object.
#' @inheritParams build_aeme
#' @returns Invisibly returns the hypsograph data frame if aeme is NULL, otherwise returns the Aeme object.
#' @importFrom cli cli_abort
#' @noRd
#' @importFrom dplyr arrange desc

check_hypsograph <- function(hypsograph, aeme = NULL) {
  if (!is.null(aeme)) {
    inp <- input(aeme)
    hypsograph <- inp$hypsograph
    if (is.null(hypsograph)) {
      cli::cli_abort(
        c(
          "!" = "No hypsograph found in {.arg aeme$input}.",
          "i" = "Please add a hypsograph data frame to the {.arg input} slot before proceeding."
        ),
        class = "aeme_error_hypsograph_missing"
      )
    }
  }
  
  # Validate data frame
  abort_if_not_dataframe(hypsograph, name = "hypsograph")
  abort_if_missing_cols(hypsograph, c("depth", "area", "elev"), name = "hypsograph")
  
  # Ensure depth descending and area ascending
  hypsograph <- dplyr::arrange(hypsograph, dplyr::desc(depth))
  
  if (any(diff(hypsograph$depth) >= 0)) {
    cli::cli_abort("{.arg depth} must be strictly monotonic decreasing.",
                   class = "aeme_error_hypsograph_depth")
  }
  if (any(diff(hypsograph$area) >= 0)) {
    cli::cli_abort("{.arg area} must be strictly monotonic increasing.",
                   class = "aeme_error_hypsograph_area")
  }
  
  invisible(if (is.null(aeme)) hypsograph else aeme)
}

#' Check if object is a valid Aeme object
#' @param aeme object to check
#' @returns Invisibly returns the Aeme object if valid, otherwise throws an 
#' error.
#' @importFrom cli cli_abort
#' @importFrom methods slotNames
#' @export
check_aeme <- function(aeme) {
  if (!inherits(aeme, "Aeme")) {
    cli::cli_abort(
      "{.arg aeme} must be an {.cls Aeme} object, not {.cls {class(aeme)[1]}}.",
      class = "aeme_error_aeme_type"
    )
  }
  
  required_slots <- c("lake", "time", "input", "inflows", "outflows", 
                      "water_balance", "parameters")
  missing_slots <- setdiff(required_slots, methods::slotNames(aeme))
  
  if (length(missing_slots) > 0) {
    cli::cli_abort(
      c(
        "!" = "The {.cls Aeme} object is missing required slots.",
        "x" = "Missing: {paste(missing_slots, collapse = ', ')}",
        "i" = "Expected slots: {paste(required_slots, collapse = ', ')}"
      ),
      class = "aeme_error_aeme_slots"
    )
  }
  
  invisible(aeme)
}


#' Check meteorological data frame
#' @param met data frame 
#' @returns Invisibly returns the met data frame if valid, otherwise throws an 
#' error.
#' @importFrom cli cli_abort
#' @noRd
check_met <- function(met) {
  # Validate data frame and required columns
  abort_if_not_dataframe(met, name = "met")
  abort_if_missing_cols(met, c("Date", "MET_radswd", "MET_tmpair", "MET_pprain"), name = "met")

  # Check wind columns
  wind1 <- "MET_wndspd"
  wind2 <- c("MET_wnduvu", "MET_wnduvv")
  if (!wind1 %in% colnames(met) && !all(wind2 %in% colnames(met))) {
    cli::cli_abort(
      c(
        "!" = "{.arg met} must contain either:",
        "*" = "{.val MET_wndspd}",
        "or" = "both {.val MET_wnduvu} and {.val MET_wnduvv}."
      ),
      class = "aeme_error_met_wind"
    )
  }

  # Check for missing values in required columns
  if (any(is.na(met[, c("Date", "MET_radswd", "MET_tmpair", "MET_pprain")]))) {
    cli::cli_abort("Missing values detected in required meteorological columns.",
                   class = "aeme_error_met_na")
  }

  # Check Date column type
  if (!inherits(met$Date, "Date")) {
    cli::cli_abort("{.arg met$Date} must be a {.cls Date} object, not {.cls {class(met$Date)[1]}}.",
                   class = "aeme_error_met_date")
  }

  invisible(met)
}


#' Format ensemble member label
#' @param ens_n integer; ensemble member number
#' @returns character; formatted ensemble member label
#' @noRd
format_ens_label <- function(ens_n) {
  paste0("ens_", sprintf("%03d", ens_n))
}

#' Return mean sea level pressure given air temperature, elevation and station pressure.
#'
#' @param MET_prsttn A numeric vector of observed station pressure in Pa
#' @param elevation A numeric vector of elevation in m
#' @param MET_tmpair A numeric vector of air temperature in degC
#'
#' @return A numeric vector of mean sea level pressure in Pa
#'
#' @references
#' Hess SL, Introduction to theoretical meteorology, Holt Rinehart and Winston, NY 1959,
#' ch. 6.5; Stull RB, Meteorology for scientists and engineers, 2nd edition,
#' Brooks/Cole 2000, ch. 1.
#'
#' @note
#' The standard procedure for the US is to use for MET_tmpair the average
#' of the current station temperature and the station temperature from 12 hours ago.
#'
#' @examples
#' get_mean_sea_level_pressure(101226.5, 105:205, 17.19)
#'
#' @export
get_mean_sea_level_pressure <- function(prsttn, elevation, tmpair) {
  # Calculate average temperature in column of air, assuming a lapse rate
  # of 6.5 degC/km
  t_column <- tmpair + 0.0065 * elevation / 2
  # Determine the scale height
  h <- 287.055 * (t_column + 273.15) / 9.807
  # Calculate the mean sea level pressure
  prsttn * exp(elevation / h)
}

#' Return station pressure from mean sea level pressure.
#'
#' @param prmslp A numeric vector of mean sea level pressure in Pa
#' @param elevation A numeric vector of elevation in m
#' @param tmpair A numeric vector of air temperature in degC
#'
#' @return A numeric vector of station pressure in Pa
#'
#' @references See \code{\link{get_mean_sea_level_pressure}}.
#'
#' @note
#' This function is just the inverse of \code{\link{get_mean_sea_level_pressure}}.
#'
#' @examples
#' get_station_pressure(101226.5, 105:205, 17.19)
#'
#' @export
get_station_pressure <- function(prmslp, elevation, tmpair) {
  prmslp / get_mean_sea_level_pressure(1, elevation, tmpair)
}
