#' Add observations to Aeme object
#'
#' @inheritParams build_aeme
#' @param lake data frame with columns "Date", "var_aeme", "depth_from",
#' "depth_to" and "value". If NULL, no observations are added.
#' @param level data frame with columns "Date", "var_aeme" and "value". If NULL,
#' no observations are added.
#'
#' @returns Aeme object with observations added
#' @export
#'

add_obs <- function(aeme, lake = NULL, level = NULL) {

  # Check if Aeme is of "Aeme" class
  if (!inherits(aeme, "Aeme")) {
    stop("aeme must be of class 'Aeme'")
  }
  obs <- observations(aeme)

  if (!is.null(lake)) {

    # Check if lake is a data frame with columns "Date", "var_aeme", "depth_from", "depth_to" and "value"
    if (!is.data.frame(lake) || !all(c("Date", "var_aeme", "depth_from", "depth_to", "value") %in% colnames(lake))) {
      stop("lake must be a data frame with columns 'Date', 'var_aeme', 'depth_from', 'depth_to' and 'value'")
    }

    obs$lake <- lake
  }

  observations(aeme) <- obs

  return(aeme)
}
