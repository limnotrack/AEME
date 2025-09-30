#' Calculate the volume of a lake using bathymetry data or a hypsograph
#'
#' @inheritParams build_aeme
#' @param hyps data.frame with columns 'depth' and 'area'.
#' @param depth numeric. The depth to which to calculate the volume. If
#' provided, the volume will be calculated to this depth. If not provided, the
#' volume will be calculated to the maximum depth of the hypsograph.
#'
#' @return numeric. The volume of the lake in cubic meters (m^3).
#' @export
#' @examples
#' aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
#' aeme <- readRDS(aeme_file)
#' vol <- calc_lake_vol(aeme)

calc_lake_vol <- function(aeme = NULL, hyps = NULL, depth = 0) {

  if (is.null(aeme) & is.null(hyps)) {
    stop("Either aeme or hyps must be provided")
  }
  
  if (!is.null(aeme)) {
    aeme <- check_aeme(aeme)
    hyps <- input(aeme)[["hypsograph"]]
    if (is.null(hyps)) {
      stop("Hypsograph is not present in the Aeme object")
    }
  } 
  
  if (!is.data.frame(hyps)) {
    stop("Input must be a data.frame object")
  }
  if (!all(c("depth", "area") %in% names(hyps))) {
    stop("Input data.frame must have columns 'depth' and 'area'")
  }
  upper <- depth
  # if (!is.null(depth)) {
  #   upper <- min(hyps$depth) + depth
  # } else {
  #   upper <- 0
  # }
  # Fit a linear model
  model <- lm(area ~ depth, data = hyps)
  
  hyps <- hyps |>
    dplyr::filter(depth <= upper) |>
    dplyr::arrange(dplyr::desc(depth))
  
  if (!upper %in% hyps$depth) {
    # Predict new values, including extrapolated ones
    new_x <- data.frame(depth = c(upper))
    surf_area <- predict(model, newdata = new_x)
    hyps <- dplyr::bind_rows(data.frame(depth = upper, area = surf_area), hyps)
  }
  
  # Calculate the incremental volumes
  hyps$volume <- with(hyps, {
    # Calculate volumes between successive depth intervals
    vol <- (area[-nrow(hyps)] + area[-1]) / 2 * diff(abs(depth))
    c(vol, 0)  # Add a zero for the last depth as it has no next depth
  })
  
  # Sum the incremental volumes to get total lake volume
  total_volume <- sum(hyps$volume)

  return(total_volume)
}
