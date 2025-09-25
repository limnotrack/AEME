#' Get observations for a given variable from an Aeme object
#'
#' @inheritParams build_aeme
#' @param var_sim character; variable in the AEME format (e.g. "HYD_temp"). Can
#' be a vector.
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

  var_sim <- check_aeme_vars(var_sim)
  # Load observations
  obs <- observations(aeme)
  tme <- time(aeme)
  df <- obs$lake |>
    dplyr::filter(var_aeme %in% var_sim,
                  Date >= as.Date(tme$start) & Date <= as.Date(tme$stop))

  if (!is.null(depth_range)) {
    depth_range <- abs(depth_range)
    df2 <- df |>
      dplyr::mutate(depth_mid = (depth_from + depth_to) / 2) |>
      dplyr::filter(depth_mid >= min(depth_range) & depth_mid <= max(depth_range))
  }

  if (nrow(df) == 0) {
    warning("No observations found for the selected variable.")
  }
  return(df)
}
