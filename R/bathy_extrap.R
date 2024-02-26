#' Extend bathymetry to a greater elevation using linear extrapolation
#'
#' @param bathy dataframe; with hypsograph
#' @param z.range numeric; 0-1, representing fraction of hypsograph to be used
#' when extrapolating the bathymetry.
#' @param new.max numeric; new max depth.
#'
#' @return dataframe with extrapolated bathymetry
#' @noRd
#' @importFrom stats approx

bathy_extrap <- function(bathy, z.range = 0.2, new.max) {

  if(missing(new.max)) {new.max = max(bathy[["elev"]]) + 5}

  # depth over which to calculate slope (proportion of water column)
  z.slope <- (max(bathy[["elev"]]) - min(bathy[["elev"]])) * z.range

  # area at target depth
  a.low <- stats::approx(bathy[["elev"]], bathy[["area"]],
                         xout = max(bathy[["elev"]]) - z.slope)$y

  # m^2 per m depth
  slope <- (max(bathy[["area"]]) - a.low) / z.slope

  new.area <- round(max(bathy[["area"]]) + (new.max - max(bathy[["elev"]])) * slope)

  return(data.frame(elev = c(bathy[["elev"]], new.max),
                    area = c(bathy[["area"]], new.area)))

}
