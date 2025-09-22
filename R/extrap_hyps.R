#' Extend hypsometry to a greater elevation using linear extrapolation
#'
#' @param hypsograph dataframe; with hypsograph
#' @inheritParams generate_hypsograph
#'
#' @return dataframe with extrapolated hypsmetry
#' @export
#' @importFrom dplyr filter bind_rows

extrap_hyps <- function(hypsograph, z_range = 0.2, ext_elev) {

  if (missing(ext_elev)) stop("ext_elev must be provided")

  # depth over which to calculate slope (proportion of water column)
  z_slope <- (max(hypsograph[["elev"]]) - min(hypsograph[["elev"]])) * z_range
  new_elev <- max(hypsograph[["elev"]]) + ext_elev

  hyps_sub <- hypsograph |>
    dplyr::filter(depth <= z_slope)
  if (nrow(hyps_sub) < 2) {
    hyps_sub <- hypsograph
  }

  # Fit a linear model to the top z_slope of the hypsograph
  fit <- lm(area ~ elev, data = hyps_sub)
  ext_hyps <- data.frame(elev = new_elev, depth = ext_elev)
  ext_hyps$area <- predict(fit, newdata = ext_hyps)

  df <- ext_hyps |>
    dplyr::bind_rows(hypsograph)

  return(df)
}
