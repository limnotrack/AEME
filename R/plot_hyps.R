#' Plot hypsograph
#'
#' @inheritParams build_aeme
#' @param y Character; column name in hypsograph data to plot on y-axis. Can
#'  be either 'elev' or 'depth'.
#' @param add_surface Logical; if TRUE, add a horizontal line representing the
#' lake surface elevation. Default is FALSE.
#' @param incl_ext_elev Logical; if TRUE, include the external elevation in the
#' hypsograph plot. Default is FALSE.
#'
#'
#' @importFrom ggplot2 ggplot aes scale_linetype_manual guides geom_line
#' geom_point labs geom_hline
#'
#' @return ggplot object
#' @export
#' @examples
#' aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
#' aeme <- readRDS(aeme_file)
#' plot_hyps(aeme = aeme)
plot_hyps <- function(aeme, y = "elev", add_surface = FALSE,
                      incl_ext_elev = FALSE) {
  # Check if aeme is a Aeme object
  if (!inherits(aeme, "Aeme")) {
    stop("aeme must be a Aeme object")
  }
  # Load input slot
  inp <- input(aeme)
  if (!is.data.frame(inp$hypsograph)) {
    stop("No hypsograph data found in input slot")
  }
  hyps <- inp$hypsograph
  if (!incl_ext_elev) {
    hyps <- hyps |>
      dplyr::filter(depth <= 0)
  }

  # Check if y is a column in hyps
  if (!y %in% names(hyps)) {
    stop("Column ", y, " not found in hypsograph data")
  }

  if (y == "elev") {
    lke <- lake(aeme)
    lake_surf <- data.frame(surf = lke$elevation)
    y_lab <- "Elevation (m)"
  } else if (y == "depth") {
    lake_surf <- data.frame(surf = 0)
    y_lab <- "Depth (m)"
  } else {
    stop("y must be either 'elev' or 'depth'")
  }
  lake_surf$name = "Surface"

  # Plot hypsograph
  p <- ggplot2::ggplot(hyps, ggplot2::aes(x = area, y = .data[[y]])) +
    ggplot2::scale_linetype_manual(values = c("dashed")) +
    ggplot2::guides(linetype = ggplot2::guide_legend(title = "")) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(y = y_lab, x = bquote("Area (m"^2~")"))

  if (add_surface) {
    p <- p +
      ggplot2::geom_hline(data = lake_surf,
                          ggplot2::aes(linetype = name,
                                       yintercept = surf))
  }
  return(p)
}
