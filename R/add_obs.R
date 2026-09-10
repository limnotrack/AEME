#' Add observations to Aeme object
#'
#' @inheritParams build_aeme
#' @param lake data frame with required columns "Date", "var_aeme", "depth" and
#' "value", and optional columns "depth_to" (bottom of an integrated sample) and
#' "sd" (measurement standard deviation, in the variable's units). The legacy
#' "depth_from" / "depth_to" column pair is accepted and collapsed to "depth"
#' (with a one-time deprecation warning). "Date" may be a `Date`, `POSIXct` or
#' date/datetime string; it is stored as a UTC `POSIXct` with daily rows
#' anchored at 12:00:00. If NULL, no observations are added.
#' @param level data frame with columns "Date", "var_aeme" and "value". "Date"
#' is handled as for `lake`. If NULL, no observations are added.
#'
#' @returns Aeme object with observations added
#' @export
#'

add_obs <- function(aeme, lake = NULL, level = NULL) {

  # Check if aeme is a Aeme object
  aeme <- check_aeme(aeme)
  obs <- observations(aeme)

  if (!is.null(lake)) {

    if (!is.data.frame(lake)) {
      stop("lake must be a data frame with columns 'Date', 'var_aeme', 'depth' and 'value'")
    }

    # Accept the legacy depth_from / depth_to layout
    lake <- normalise_lake_obs(lake)

    if (!all(get_obs_column_names() %in% colnames(lake))) {
      stop("lake must be a data frame with columns 'Date', 'var_aeme', 'depth' and 'value'")
    }

    # Store Date as noon-anchored UTC POSIXct (daily rows at 12:00:00) so it
    # matches obs already on the object and a sub-daily model axis.
    if (!inherits(lake$Date, c("POSIXct", "Date"))) {
      cli::cli_warn(
        c("!" = "{.arg lake$Date} is not {.cls POSIXct} or {.cls Date}.",
          "i" = "Parsing it as {.val UTC} and anchoring daily rows at 12:00:00."),
        class = "aeme_warn_obs_date_coerced"
      )
    }
    lake$Date <- .as_obs_datetime(lake$Date)

    orig_data <- obs$lake

    # Combine existing and new lake data, avoiding duplicates
    if (!is.null(orig_data)) {
      combined_data <- dplyr::bind_rows(orig_data, lake) |>
        dplyr::distinct(Date, var_aeme, depth,
                        dplyr::across(dplyr::any_of("depth_to")),
                        .keep_all = TRUE) |>
        dplyr::arrange(Date, var_aeme, depth)
      obs$lake <- combined_data
    } else {
      obs$lake <- lake
    }

  }
  
  if (!is.null(level)) {

    # Check if level is a data frame with columns "Date", "var_aeme" and "value"
    if (!is.data.frame(level) || !all(c("Date", "var_aeme", "value") %in% colnames(level))) {
      stop("level must be a data frame with columns 'Date', 'var_aeme' and 'value'")
    }

    if (!inherits(level$Date, c("POSIXct", "Date"))) {
      cli::cli_warn(
        c("!" = "{.arg level$Date} is not {.cls POSIXct} or {.cls Date}.",
          "i" = "Parsing it as {.val UTC} and anchoring daily rows at 12:00:00."),
        class = "aeme_warn_obs_date_coerced"
      )
    }
    level$Date <- .as_obs_datetime(level$Date)

    orig_data <- obs$level
    # Combine existing and new level data, avoiding duplicates
    if (!is.null(orig_data)) {
      combined_data <- dplyr::bind_rows(orig_data, level) |>
        dplyr::distinct(Date, var_aeme, .keep_all = TRUE) |> 
        dplyr::arrange(Date, var_aeme)
      obs$level <- combined_data
    } else {
      obs$level <- level
    }

  }

  observations(aeme) <- obs
  return(aeme)
}
