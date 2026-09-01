#' Get observations for a given variable from an Aeme object
#'
#' @inheritParams build_aeme
#' @param var_sim character; variable in the AEME format (e.g. "HYD_temp"). Can
#' be a vector. If missing, all variables are returned.
#' @param depth_range numeric vector of length 2; depth range (in meters) to
#' filter observations. If NULL, all depths are returned.
#' @param time_filter logical; if TRUE, filter observations to the time range of
#' the Aeme object. If FALSE, all observations are returned regardless of time.
#'
#' @return A data frame with the following columns:
#' \itemize{
#' \item \code{Date}: Date of observation
#' \item \code{var_aeme}: Name of the variable in the AEME format
#' \item \code{depth}: Nominal sampling depth (m, positive-down from the surface)
#' \item \code{value}: Value of the variable
#' \item \code{depth_to}: (optional) Bottom of an integrated sample, if recorded
#' \item \code{sd}: (optional) Measurement standard deviation, in the variable's
#' units
#' }
#' @export
#'

get_obs <- function(aeme, var_sim, depth_range = NULL, time_filter = FALSE) {

  # Load observations
  obs <- observations(aeme)
  obs_col_names <- get_obs_column_names(include_optional = TRUE)
  if (missing(var_sim)) {
    # If var_sim is missing, return all observations
    var_sim <- c(unique(obs$lake$var_aeme), "LKE_lvlwtr")
  } else {
    var_sim <- check_aeme_vars(var_sim, aeme = aeme)
  }
  if (!is.null(obs$lake)) {
    lake <- obs$lake |>
      dplyr::filter(var_aeme %in% var_sim) |>
      dplyr::select(dplyr::any_of(obs_col_names))
  } else {
    lake <- NULL
  }

  if (!is.null(obs$level) & "LKE_lvlwtr" %in% var_sim) {
    level <- obs$level |>
      dplyr::mutate(depth = NA_real_) |>
      dplyr::select(dplyr::any_of(obs_col_names))
  } else {
    level <- NULL
  }

  if (!is.null(depth_range)) {
    depth_range <- abs(depth_range)
    lake <- lake |>
      dplyr::filter(depth >= min(depth_range) & depth <= max(depth_range))
  }

  df <- dplyr::bind_rows(lake, level)

  if (nrow(df) == 0) {
    warning("No observations found for the selected variable.")
  } else {
    df <- df  |>
      dplyr::arrange(Date, var_aeme, depth)
  }
  
  if (time_filter) {
    tme <- time(aeme)
    df <- df |> 
      dplyr::filter(Date >= as.Date(tme$start) & Date <= as.Date(tme$stop))
  }
  return(df)
}
