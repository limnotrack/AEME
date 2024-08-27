#' Generate a hypsograph curve
#'
#' This function generates a hypsograph curve for a lake based on the maximum
#' depth, surface area, and volume development factor.
#'
#' @param max_depth The maximum depth of the lake.
#' @param surface_area The surface area of the lake.
#' @param volume_development The volume development factor.
#' @param elev The elevation of the lake. If not provided, it is assumed to be
#' the same as the maximum depth.
#'
#' @return A data frame with columns for elevation, depth, and area.
#' @export
#'
#' @importFrom utils data
#' @importFrom dplyr filter pull
#'

generate_hypsograph <- function(max_depth, surface_area,
                                volume_development = 1.5, elev = NULL,
                                ext_elev = 0) {

  utils::data("model_layer_structure", package = "AEME", envir = environment())

  if (is.null(elev)) {
    elev <- max_depth
  }

  # Generate a sequence of depths from 0 to the maximum depth
  depths <- model_layer_structure |>
    dplyr::filter(zi <= max_depth) |>
    dplyr::pull(zi)
  if (!max_depth %in% depths) {
    depths <- c(depths, max_depth)
  }

  # Estimate the relative area at each depth
  relative_area <- (1 - (depths / max_depth))^volume_development

  # Convert relative area to actual area
  areas <- relative_area * surface_area

  # Create the hypsograph data frame
  hyps <- data.frame(elev = elev - depths, depth = depths, area = areas)

  if (ext_elev > 0) {
    hyps <- extrap_hyps(hyps = hyps, ext_elev = ext_elev)
  }
  hyps <- hyps |>
    dplyr::mutate(depth = -depth)

  return(hyps)
}
