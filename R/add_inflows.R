#' Add inflows to Aeme object
#'
#' @inheritParams build_aeme
#' @param data list with data frames for each inflow. Each data frame must have
#' columns "Date", "HYD_flow", "HYD_temp" and "CHM_salt". If NULL, no inflows
#' are added.
#'
#' @returns Aeme object with inflows added
#' @export
#'

add_inflows <- function(aeme, data) {
  # Check if Aeme is of "Aeme" class
  if (!inherits(aeme, "Aeme")) {
    stop("aeme must be of class 'Aeme'")
  }

  if (!is.list(data)) {
    stop("data must be a list")
  }

  inf <- inflows(aeme)
  inf$data <- data
  inflows(aeme) <- inf
  return(aeme)
}
