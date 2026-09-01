#' Add observations to Aeme object
#'
#' @inheritParams build_aeme
#' @param lake data frame with required columns "Date", "var_aeme", "depth" and
#' "value", and optional columns "depth_to" (bottom of an integrated sample) and
#' "sd" (measurement standard deviation, in the variable's units). The legacy
#' "depth_from" / "depth_to" column pair is accepted and collapsed to "depth"
#' (with a one-time deprecation warning). If NULL, no observations are added.
#' @param level data frame with columns "Date", "var_aeme" and "value". If NULL,
#' no observations are added.
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
