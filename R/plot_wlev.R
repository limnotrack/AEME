#' Plot lake water level
#'
#' @inheritParams plot_output
#' @param ... Additional arguments passed to \code{plot_output}
#'
#' @returns A ggplot object
#' @export
#'

plot_wlev <- function(aeme, model, facet = FALSE, ...) {
  plot_output(aeme = aeme, model = model, var_sim = "LKE_lvlwtr", facet = facet, 
              ...) +
    ggplot2::ylab("Water level (m)")
}
