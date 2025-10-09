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
#' 
#' @importFrom dplyr select mutate left_join rename
#' @importFrom units as_units set_units
#'
#' @returns A data frame formatted for AEME with columns "Date", "var_aeme",
#' "depth_from", "depth_to", and "value".
#' @export
#'

lake_obs_to_aeme <- function(data, depth_col_name, datetime_col_name,
                             var_col_name, value_col_name, lake_id_col, 
                             var_map) {
  
  # Load Rdata
  data("key_naming", package = "AEME", envir = environment())
  sub_key_naming <- key_naming |> 
    dplyr::select(name, units) |> 
    dplyr::rename(aeme_units = units)
  
  # Check data is a data frame
  if (!is.data.frame(data)) {
    stop("Data must be a data frame.")
  }
  
  # Variable
  if (missing(var_col_name)) {
    var_col_name <- names(data)[grepl("var|parameter|param", names(data), 
                                      ignore.case = TRUE)]
    if (length(var_col_name) == 0) {
      stop("Variable column name not provided and could not be inferred.")
    } else if (length(var_col_name) > 1) {
      stop("Multiple variable column names inferred: c(\"", 
           paste0(var_col_name, collapse = "\", "),
           "\"). Please specify one.")
    }
  } else {
    # Check variable column is in data
    if (!var_col_name %in% names(data)) {
      stop("Provided var_col_name '", var_col_name, 
           "' not found in data.")
    }
    # Check variable column is character
    if (!is.character(data[[var_col_name]])) {
      stop("Variable column '", var_col_name, "' must be character.")
    }
  }
  
  # Check var_map is a dataframe
  if (!is.data.frame(var_map)) {
    stop("var_map must be a data frame.")
  } else {
    # Check var_map has required columns
    req_cols <- c("var_aeme", "name", "unit")
    if (!all(req_cols %in% names(var_map))) {
      missing_cols <- req_cols[!req_cols %in% names(var_map)]
      stop("var_map is missing required columns: ", 
           paste(missing_cols, collapse = ", "))
    }
    
    # Check var_aeme in var_map is in sub_key_naming
    if (!all(var_map$var_aeme %in% sub_key_naming$name)) {
      wrong_vars <- var_map$var_aeme[!var_map$var_aeme %in% sub_key_naming$name]
      stop("The following var_aeme in var_map are not in key_naming: ", 
           paste(wrong_vars, collapse = ", "))
    }
    
    # Check name in var_map in data
    if (!all(var_map$name %in% data[[var_col_name]])) {
      wrong_names <- var_map$name[!var_map$name %in% data[[var_col_name]]]
      stop("The following names in var_map are not in data: ", 
           paste(wrong_names, collapse = ", "))
    }
    
    # Check all units in var_map are valid in the units package
    invalid_units <- var_map$unit[!sapply(var_map$unit, function(u) {
      tryCatch({
        units::as_units(u)
        TRUE
      }, error = function(e) FALSE)
    })]
    if (length(invalid_units) > 0) {
      stop("The following units in var_map are not valid: ", 
           paste(unique(invalid_units), collapse = ", "))
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
      stop("Date column name not provided and could not be inferred.")
    } else if (length(datetime_col_name) > 1) {
      stop("Multiple date column names inferred: c(\"", 
           paste0(datetime_col_name, collapse = "\", "), 
           "\"). Please specify one.")
    }
  } else {
    # Check datetime column is in data
    if (!datetime_col_name %in% names(data)) {
      stop("Provided datetime_col_name '", datetime_col_name, 
           "' not found in data.")
    }
    # Check datetime column is POSIXct
    if (!inherits(data[[datetime_col_name]], "POSIXct")) {
      stop("Datetime column '", datetime_col_name, "' must be of class POSIXct.")
    }
  }
  
  # Depth
  if (missing(depth_col_name)) {
    depth_col_name <- names(data)[grepl("depth", names(data), 
                                        ignore.case = TRUE)]
    if (length(depth_col_name) == 0) {
      stop("Depth column name not provided and could not be inferred.")
    } else if (length(depth_col_name) > 1) {
      stop("Multiple depth column names inferred: c(\"", 
           paste0(depth_col_name, collapse = "\", "),
           "\"). Please specify one.")
    }
  } else {
    # Check depth column is in data
    if (!depth_col_name %in% names(data)) {
      stop("Provided depth_col_name '", depth_col_name, 
           "' not found in data.")
    }
    # Check depth column is numeric
    if (!is.numeric(data[[depth_col_name]])) {
      stop("Depth column '", depth_col_name, "' must be numeric.")
    }
  }
  
  if (missing(value_col_name)) {
    value_col_name <- names(data)[grepl("value|obs", names(data), 
                                        ignore.case = TRUE)]
    if (length(value_col_name) == 0) {
      stop("Value column name not provided and could not be inferred.")
    } else if (length(value_col_name) > 1) {
      stop("Multiple value column names inferred: c(\"", 
           paste0(value_col_name, collapse = "\", "), 
           "\"). Please specify one.")
    }
  } else {
    # Check value column is in data
    if (!value_col_name %in% names(data)) {
      stop("Provided value_col_name '", value_col_name, 
           "' not found in data.")
    }
    # Check value column is numeric
    if (!is.numeric(data[[value_col_name]])) {
      stop("Value column '", value_col_name, "' must be numeric.")
    }
  }
  
  if (missing(lake_id_col)) {
    remove_lake_id <- TRUE
    lake_id_col <- "lake_id"
    data[[lake_id_col]] <- "lake_1"
    warning(strwrap("lake_id_col not provided. Using default 'lake_id' with all
                     values set to 'lake_1'."))
  } else {
    remove_lake_id <- FALSE
  }
  
  
  obs <- data |> 
    dplyr::select(dplyr::all_of(c(lake_id_col, datetime_col_name, 
                                  depth_col_name, var_col_name, 
                                  value_col_name))) |> 
    dplyr::rename(depth_from = dplyr::all_of(depth_col_name),
                  datetime = dplyr::all_of(datetime_col_name),
                  var = dplyr::all_of(var_col_name),
                  value = dplyr::all_of(value_col_name),
                  lake_id = dplyr::all_of(lake_id_col)) |> 
    dplyr::left_join(var_map, by = c("var" = "name")) |> 
    dplyr::left_join(sub_key_naming, by = c("var_aeme" = "name")) |>
    dplyr::filter(!is.na(var_aeme), !is.na(unit)) |>
    dplyr::mutate(Date = as.Date(datetime),
                  depth_to = depth_from) 
  
  # Group by var_aeme and convert units
  v <- var_map$var_aeme[9]
  obs_col_names <- AEME::get_obs_column_names()
  out <- lapply(var_map$var_aeme, \(v) {
    df <- obs |> 
      dplyr::filter(var_aeme == v)
    if (nrow(df) == 0) return(NULL)
    uniq_units <- unique(df$unit) 
    if (length(uniq_units) > 1) {
      stop("Multiple units found for variable '", v, 
              "': ", paste(uniq_units, collapse = ", "), 
              ". Converting all to first unit: '", uniq_units[1], "'.")
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
        warning("Could not convert units from '", uniq_units, "' to '", 
             df$aeme_units[1], "' for variable '", v, 
             "'. Removing these observations.")
        return(NULL)
      }
      
      df <- df |> 
        dplyr::mutate(value = value * conv_factor,
                      unit = df$aeme_units[1])
      return(df)
    }
  }) |> 
    dplyr::bind_rows() |> 
    dplyr::select(dplyr::all_of(c(obs_col_names, "lake_id", "unit"))) |> 
    dplyr::arrange(lake_id, Date, var_aeme, depth_from)
  
  if (remove_lake_id) {
    out <- out |> 
      dplyr::select(-lake_id)
  }
  
  return(out)
}
