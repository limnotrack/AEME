#' Extend bathymetry to a greater elevation using linear extrapolation
#'
#' @param bathy
#' @param z.range
#' @param new.max
#'
#' @return
#' @noRd

bathy_extrap <- function(bathy, z.range = 0.2, new.max) {

  if(missing(new.max)) {new.max = max(bathy[,1]) + 5}

  # depth over which to calculate slope (proportion of water column)
  z.slope <- (max(bathy[,1]) - min(bathy[,1])) * z.range

  # area at target depth
  a.low <- approx(bathy[, 1], bathy[, 2], xout = max(bathy[, 1]) - z.slope)$y

  # m^2 per m depth
  slope <- (max(bathy[,2]) - a.low) / z.slope

  new.area <- round(max(bathy[,2]) + (new.max - max(bathy[,1])) * slope)

  return(data.frame(elev = c(bathy[,1], new.max),
                    area = c(bathy[,2], new.area)))

}
