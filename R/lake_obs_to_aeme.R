#' Format lake observation data to AEME format
#' 
#' Converts a data frame of lake observations into the format required for AEME.
#' The function maps variable names and units to AEME standards using provided 
#' lookup tables. It also ensures that required columns are present and 
#' correctly named. 
#'
#' @param data data frame containing lake observations
#' @param depth_col_name column name for depth (m). If missing, the function
#'  will attempt to infer it.
#' @param depth_to_col_name column name for the bottom of an integrated sample
#'  (m). Optional; if missing, no `depth_to` column is produced.
#' @param sd_col_name column name for the measurement standard deviation, in the
#'  input units of each variable. Optional; if missing, no `sd` column is
#'  produced.
#' @param datetime_col_name column name for date/time. If missing, the function
#' will attempt to infer it.
#' @param var_col_name column name for variable names. If missing, the function
#' will attempt to infer it.
#' @param value_col_name column name for variable values. If missing, the 
#' function will attempt to infer it.
#' @param lake_id_col column name for lake identifier. If missing, the function
#' assumes all data is for a single lake.
#' @param var_map data frame with columns "var_aeme", "name", and "unit" for
#' mapping variable names and units to AEME standards. "var_aeme" is the
#' defined AEME variable name, "name" is the name used in the input data,
#' and "unit" is the unit used in the input data.
#' @param tz character; Olson timezone a naive datetime column is expressed in.
#' Sub-daily timestamps are converted to UTC and kept at their time-of-day;
#' daily data is treated as calendar dates (never shifted) and anchored at
#' 12:00:00 UTC. Default `"UTC"`.
#'
#' @importFrom dplyr select mutate left_join rename
#' @importFrom units as_units set_units
#'
#' @returns A data frame formatted for AEME with required columns "Date"
#' (UTC `POSIXct`; daily observations anchored at 12:00:00), "var_aeme",
#' "depth", and "value", plus the optional columns "depth_to" and "sd" when
#' the corresponding arguments are supplied.
#' @export
#'

lake_obs_to_aeme <- function(data, depth_col_name, datetime_col_name,
                             var_col_name, value_col_name, lake_id_col,
                             var_map, depth_to_col_name, sd_col_name,
                             tz = "UTC") {

  # Internal datetime arithmetic runs in UTC. A naive datetime column is taken
  # to be wall-clock time in `tz`; a Date (or tz-aware POSIXct) is left as the
  # calendar/absolute value it already is.
  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")

  # Load Rdata
  data("key_naming", package = "AEME", envir = environment())
  sub_key_naming <- key_naming |> 
    dplyr::select(var_aeme, units) |> 
    dplyr::rename(aeme_units = units)
  
  # Check data is a data frame
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }
  
  # Variable
  if (missing(var_col_name)) {
    var_col_name <- names(data)[grepl("var|parameter|param", names(data), 
                                      ignore.case = TRUE)]
    if (length(var_col_name) == 0) {
      cli::cli_abort("Variable column name not provided and could not be 
                     inferred.")
    } else if (length(var_col_name) > 1) {
      cli::cli_abort("Multiple variable column names inferred: 
      {.var {var_col_name}}. Please specify one.")
    }
  } else {
    # Check variable column is in data
    if (!var_col_name %in% names(data)) {
      cli::cli_abort("Provided var_col_name '{.var {var_col_name}}' not found in data.")
    }
    # Check variable column is character
    if (!is.character(data[[var_col_name]])) {
      cli::cli_abort("Variable column '{.var {var_col_name}}' must be character.")
    }
  }
  
  # Check var_map is a dataframe
  if (!is.data.frame(var_map)) {
    cli::cli_abort("{.arg var_map} must be a data frame.")
  } else {
    # Check var_map has required columns
    req_cols <- c("var_aeme", "name", "unit")
    if (!all(req_cols %in% names(var_map))) {
      missing_cols <- req_cols[!req_cols %in% names(var_map)]
      cli::cli_abort("{.arg var_map} is missing required columns: 
                     {.var {missing_cols}}.")
    }
    
    # Check var_aeme in var_map is in sub_key_naming
    if (!all(var_map$var_aeme %in% sub_key_naming$var_aeme)) {
      wrong_vars <- var_map$var_aeme[!var_map$var_aeme %in% sub_key_naming$var_aeme]
      cli::cli_abort("The following var_aeme in var_map are not in key_naming: 
                     {.var {wrong_vars}}.")
    }
    
    # Check name in var_map in data
    if (!all(var_map$name %in% data[[var_col_name]])) {
      wrong_names <- var_map$name[!var_map$name %in% data[[var_col_name]]]
      cli::cli_abort("The following names in var_map are not in data: 
                     {.var {wrong_names}}.")
    }
    
    # Check all units in var_map are valid in the units package
    invalid_units <- var_map$unit[!sapply(var_map$unit, function(u) {
      tryCatch({
        units::as_units(u)
        TRUE
      }, error = function(e) FALSE)
    })]
    if (length(invalid_units) > 0) {
      cli::cli_abort("The following units in {.arg var_map} are not valid: 
                     {.var {unique(invalid_units)}}.")
    }
  }
  
  # Check var_lookup is a named vector
  # if (!is.character(var_lookup) || is.null(names(var_lookup))) {
  #   stop("var_lookup must be a named character vector.")
  # }
  # 
  # # Check unit_lookup is a named vector
  # if (!is.character(unit_lookup) || is.null(names(unit_lookup))) {
  #   stop("unit_lookup must be a named character vector.")
  # }
  # 
  # # Check var_lookup is in key_naming
  # if (!all(var_lookup %in% key_naming$name)) {
  #   wrong_vars <- var_lookup[!var_lookup %in% key_naming$name]
  #   stop("The following variable names in var_lookup are not in key_naming: ", 
  #        paste(wrong_vars, collapse = ", "))
  # }
  
  # Infer column names if not provided ----
  # Datetime
  if (missing(datetime_col_name)) {
    datetime_col_name <- names(data)[grepl("date|time", names(data), 
                                           ignore.case = TRUE)]
    if (length(datetime_col_name) == 0) {
      cli::cli_abort("Datetime column name not provided and could not be inferred.")
    } else if (length(datetime_col_name) > 1) {
      cli::cli_abort("Multiple datetime column names inferred: 
                     {.var {datetime_col_name}}. Please specify one.")
    }
  } else {
    # Check datetime column is in data
    if (!datetime_col_name %in% names(data)) {
      cli::cli_abort("Provided datetime_col_name '{.var {datetime_col_name}}' 
                     not found in data.")
    }
    # Check datetime column is POSIXct or Date
    if (!inherits(data[[datetime_col_name]], c("POSIXct", "Date"))) {
      cli::cli_abort("Datetime column {.val {datetime_col_name}} must be of 
                     class POSIXct or Date.")
    }
  }
  
  # Depth
  if (missing(depth_col_name)) {
    depth_col_name <- names(data)[grepl("depth", names(data), 
                                        ignore.case = TRUE)]
    if (length(depth_col_name) == 0) {
      cli::cli_abort("Depth column name not provided and could not be inferred.")
    } else if (length(depth_col_name) > 1) {
      cli::cli_abort("Multiple depth column names inferred: 
                     {.var {depth_col_name}}. Please specify one.")
    }
  } else {
    # Check depth column is in data     
    if (!depth_col_name %in% names(data)) {
      cli::cli_abort("Provided depth_col_name '{.var {depth_col_name}}' not found in data.")
    }
    # Check depth column is numeric
    if (!is.numeric(data[[depth_col_name]])) {
      cli::cli_abort("Depth column '{.var {depth_col_name}}' must be numeric.")
    }
  }

  # Optional depth_to / sd columns
  extra_cols <- character()
  if (!missing(depth_to_col_name)) {
    if (!depth_to_col_name %in% names(data)) {
      cli::cli_abort("Provided depth_to_col_name '{.var {depth_to_col_name}}' not found in data.")
    }
    if (!is.numeric(data[[depth_to_col_name]])) {
      cli::cli_abort("depth_to column '{.var {depth_to_col_name}}' must be numeric.")
    }
    extra_cols <- c(extra_cols, depth_to = depth_to_col_name)
  }
  if (!missing(sd_col_name)) {
    if (!sd_col_name %in% names(data)) {
      cli::cli_abort("Provided sd_col_name '{.var {sd_col_name}}' not found in data.")
    }
    if (!is.numeric(data[[sd_col_name]])) {
      cli::cli_abort("sd column '{.var {sd_col_name}}' must be numeric.")
    }
    extra_cols <- c(extra_cols, sd = sd_col_name)
  }

  if (missing(value_col_name)) {
    value_col_name <- names(data)[grepl("value|obs", names(data), 
                                        ignore.case = TRUE)]
    if (length(value_col_name) == 0) {
      cli::cli_abort("Value column name not provided and could not be inferred.")
    } else if (length(value_col_name) > 1) {
      cli::cli_abort("Multiple value column names inferred: {.var {value_col_name}}. Please specify one.")
    }
  } else {
    # Check value column is in data
    if (!value_col_name %in% names(data)) {
      cli::cli_abort("Provided value_col_name '{.var {value_col_name}}' not found in data.")
    }
    # Check value column is numeric
    if (!is.numeric(data[[value_col_name]])) {
      cli::cli_abort("Value column '{.var {value_col_name}}' must be numeric.")
    }
  }
  
  if (missing(lake_id_col)) {
    remove_lake_id <- TRUE
    lake_id_col <- "lake_id"
    data[[lake_id_col]] <- "lake_1"
    warning(strwrap("lake_id_col not provided. Assuming all data is for a single
                    lake.."))
  } else {
    remove_lake_id <- FALSE
  }
  
  
  obs <- data |>
    dplyr::select(dplyr::all_of(c(lake_id_col, datetime_col_name,
                                  depth_col_name, var_col_name,
                                  value_col_name, extra_cols))) |>
    dplyr::rename(depth = dplyr::all_of(depth_col_name),
                  datetime = dplyr::all_of(datetime_col_name),
                  var = dplyr::all_of(var_col_name),
                  value = dplyr::all_of(value_col_name),
                  lake_id = dplyr::all_of(lake_id_col)) |>
    dplyr::left_join(var_map, by = c("var" = "name")) |>
    dplyr::left_join(sub_key_naming, by = c("var_aeme")) |>
    # dplyr::rename(var_aeme = var) |>
    dplyr::filter(!is.na(var_aeme), !is.na(unit)) |>
    dplyr::mutate(Date = .as_obs_datetime(datetime, tz = tz,
                                          reinterpret_utc_tag = TRUE))

  # Group by var_aeme and convert units
  v <- var_map$var_aeme[9]
  obs_col_names <- AEME::get_obs_column_names(include_optional = TRUE)
  out <- lapply(var_map$var_aeme, \(v) {
    df <- obs |> 
      dplyr::filter(var_aeme == v)
    if (nrow(df) == 0) return(NULL)
    uniq_units <- unique(df$unit) 
    if (length(uniq_units) > 1) {
      cli::cli_warn("Multiple units found for variable '{.var {v}}': 
                     {.var {paste(uniq_units, collapse = ', ')}}. Converting all
                     to first unit: '{.var {uniq_units[1]}}'.")
    }
    if (uniq_units == df$aeme_units[1]) {
      return(df)
    } else {
      from_unit <- units::as_units(uniq_units)
      to_unit <- units::as_units(as.character(df$aeme_units[1]))
      conv_factor <- tryCatch({
        as.numeric(units::set_units(from_unit, to_unit, mode = "standard"))
      }, error = function(e) {
        NA
        # stop("Error converting units from '", uniq_units, "' to '", 
        #      df$aeme_units[1], "' for variable '", v, "': ", e$message)
      })
      if (is.na(conv_factor)) {
        cli::cli_warn("Could not convert units from '{.var {uniq_units}}' to 
                      '{.var {df$aeme_units[1]}}' for variable '{.var {v}}'.
                      Removing these observations.")
        return(NULL)
      }
      
      df <- df |>
        dplyr::mutate(value = value * conv_factor,
                      unit = df$aeme_units[1])
      if ("sd" %in% names(df)) df$sd <- df$sd * conv_factor
      return(df)
    }
  }) |>
    dplyr::bind_rows() |>
    dplyr::select(dplyr::any_of(c(obs_col_names, "lake_id", "unit"))) |>
    dplyr::arrange(lake_id, Date, var_aeme, depth)
  
  if (remove_lake_id) {
    out <- out |> 
      dplyr::select(-lake_id)
  }
  
  return(out)
}
