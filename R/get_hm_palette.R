#' Get default heatmap palette for a variable
#'
#' @param var Character. Variable name.
#' @param n Number of colours required.
#'
#' @return A character vector of colours
#' @export
get_hm_palette <- function(var, n = NULL) {
  pal <- .hm_palettes[[var]]
  
  if (is.null(pal)) {
    # If no palette, use viridis
    pal <- .hm_palettes[["default"]]
  }
  
  if (!is.null(n) && length(pal) != n) {
    pal <- grDevices::colorRampPalette(pal)(n)
  }
  
  pal
}
