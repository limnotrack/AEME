#' Check if water temperature profile is stratified
#' @param wtr numeric vector; water temperature profile
#' @param depths numeric vector; depths corresponding to water temperature 
#' profile
#' @param t_diff numeric; minimum temperature difference between surface and
#' bottom to consider the profile stratified. Default is 1 degree Celsius.
#' @returns logical; TRUE if profile is stratified, FALSE otherwise
#' @export
is_strat <- function(wtr, depths, t_diff = 1) {
  if (length(wtr) != length(depths)) {
    cli::cli_abort("wtr and depths must have the same length")
  }
  if (any(is.na(wtr)) || any(is.na(depths))) {
    cli::cli_abort("wtr and depths must not contain NA values")
  }
  if (length(wtr) < 2) {
    return(FALSE)
  }
  surface_temp <- wtr[which.min(depths)]
  bottom_temp <- wtr[which.max(depths)]
  return(abs(surface_temp - bottom_temp) >= t_diff)
}
