#' Get model layers
#'
#' @param depth numeric; depth of the lake
#' @param thickness_factor numeric; factor to multiply the thickness of the
#' layers. Default is 1.
#'
#' @noRd

get_model_layers <- function(depth, thickness_factor = 1) {
  utils::data("model_layer_structure", package = "AEME", envir = environment())

  mod_layers <- model_layer_structure |>
    dplyr::mutate(h = h * thickness_factor,
                  zi = cumsum(c(0, h[-length(h)])),
                  z = zi + (diff(c(zi, NA))) / 2) |>
    dplyr::filter(z <= depth)
  return(mod_layers)

}
