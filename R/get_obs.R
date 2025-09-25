#' Get observations for a given variable from an Aeme object
#'
#' @inheritParams build_aeme
#' @param var_sim character; variable in the AEME format (e.g. "HYD_temp"). Can
#' be a vector. If missing, all variables are returned.
#' @param depth_range numeric vector of length 2; depth range (in meters) to
#' filter observations. If NULL, all depths are returned.
#'
#' @return A data frame with the following columns:
#' \itemize{
#' \item \code{Date}: Date of observation
#' \item \code{var_aeme}: Name of the variable in the AEME format
#' \item \code{depth_from}: Depth from which the variable is extracted
#' \item \code{depth_to}: Depth to which the variable is extracted
#' \item \code{value}: Value of the variable
#' }
#' @export
#'

get_obs <- function(aeme, var_sim, depth_range = NULL) {

  # Load observations
  obs <- observations(aeme)
  obs_col_names <- get_obs_column_names()
  if (missing(var_sim)) {
    # If var_sim is missing, return all observations
    var_sim <- c(unique(obs$lake$var_aeme), "LKE_lvlwtr")
  } else {
    var_sim <- check_aeme_vars(var_sim)
  }
  tme <- time(aeme)
  if (!is.null(obs$lake)) {
    lake <- obs$lake |>
      dplyr::filter(var_aeme %in% var_sim,
                    Date >= as.Date(tme$start) & Date <= as.Date(tme$stop)) |> 
      dplyr::select(dplyr::all_of(obs_col_names))
  } else {
    lake <- NULL
  }
  
  if (!is.null(obs$level) & "LKE_lvlwtr" %in% var_sim) {
    level <- obs$level |>
      dplyr::filter(Date >= as.Date(tme$start) & Date <= as.Date(tme$stop)) |> 
      dplyr::mutate(depth_from = NA, depth_to = NA) |> 
      dplyr::select(dplyr::all_of(obs_col_names))
  } else {
    level <- NULL
  }

  if (!is.null(depth_range)) {
    depth_range <- abs(depth_range)
    lake <- lake |>
      dplyr::mutate(depth_mid = (depth_from + depth_to) / 2) |>
      dplyr::filter(depth_mid >= min(depth_range) & depth_mid <= max(depth_range))
  }
  
  df <- dplyr::bind_rows(lake, level) |>
    dplyr::arrange(Date, var_aeme, depth_from, depth_to)

  if (nrow(df) == 0) {
    warning("No observations found for the selected variable.")
  }
  return(df)
}
