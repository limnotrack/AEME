#' Extend hypsmetry to a greater elevation using linear extrapolation
#'
#' @param hyps dataframe; with hypsograph
#' @param z_range numeric; 0-1, representing fraction of hypsograph to be used.
#' Default is 0.2, which uses the top 20% of the hypsograph for extrapolation.
#' when extrapolating the hypsmetry.
#' @param ext_elev numeric; new max depth.
#'
#' @return dataframe with extrapolated hypsmetry
#' @export
#' @importFrom stats approx

extrap_hyps <- function(hyps, z_range = 0.2, ext_elev) {

  if(missing(ext_elev)) stop("ext_elev must be provided")

  # depth over which to calculate slope (proportion of water column)
  z_slope <- (max(hyps[["elev"]]) - min(hyps[["elev"]])) * z_range
  new_elev <- max(hyps[["elev"]]) + ext_elev

  hyps_sub <- hyps |>
    dplyr::filter(depth <= z_slope)
  if (nrow(hyps_sub) < 2) {
    hyps_sub <- hyps
  }

  # plot(hyps_sub$elev, hyps_sub$area, type = "l")

  # Fit a linear model to the top z_slope of the hypsograph
  fit <- lm(area ~ elev, data = hyps_sub)
  ext_hyps <- data.frame(elev = new_elev, depth = ext_elev)
  ext_hyps$area <- predict(fit, newdata = ext_hyps)
  # plot(hyps$area, hyps$elev, type = "l", xlim = c(0, 100000), ylim = c(0, 20))
  # points(ext_hyps$area, ext_hyps$elev, col = "red")

  # m^2 per m depth
  # slope <- (max(hyps[["area"]]) - a.low) / z_slope

  # new_depth <- ext_elev
  # new_area <- round(max(hyps[["area"]]) + (ext_elev - max(hyps[["elev"]])) * slope)

  df <- ext_hyps |>
    dplyr::bind_rows(hyps)

  return(df)

}
