#' Plot lake loading summary
#'
#' Visualises the output of \code{summarise_loads()} as bar charts of annual
#' average discharge and constituent loads, decomposed by inflow.
#'
#' @inheritParams build_aeme
#' @param loads data.frame; output of \code{summarise_loads(aeme, by_inflow = TRUE)}.
#'   If \code{NULL} (default), it is calculated internally.
#' @param inflow_vars character; vector of AEME inflow concentration variable
#'   names to plot (passed to \code{summarise_loads()} when \code{loads} is
#'   \code{NULL}). If \code{NULL} (default), all recognised mass-concentration
#'   variables present in the inflow data are plotted.
#'
#' @return A ggplot2 object with one facet per variable (discharge and each
#' constituent load), showing the annual average contribution of each inflow.
#'
#' @importFrom ggplot2 ggplot aes geom_col facet_wrap labs theme_bw
#' @export
#'
#' @examples
#' aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
#' aeme <- readRDS(aeme_file)
#' plot_loads(aeme)
plot_loads <- function(aeme, loads = NULL, inflow_vars = NULL) {

  aeme <- check_aeme(aeme)
  if (is.null(loads)) {
    loads <- summarise_loads(aeme, inflow_vars = inflow_vars, by_inflow = TRUE)
  }

  # Drop the "all" summary row(s) when more than one inflow is present so
  # bars show the per-inflow breakdown; keep them when there is only one.
  if (length(unique(loads[["inflow_id"]])) > 1) {
    loads <- loads[loads[["inflow_id"]] != "all", ]
  }

  loads[["facet_lab"]] <- paste0(loads[["name_text"]], " (", loads[["unit"]],
                                 "/yr)")

  ggplot2::ggplot(loads, ggplot2::aes(x = .data[["inflow_id"]],
                                      y = .data[["annual_average"]],
                                      fill = .data[["inflow_id"]])) +
    ggplot2::geom_col() +
    ggplot2::facet_wrap(~ facet_lab, scales = "free_y") +
    ggplot2::labs(x = "Inflow", y = "Annual average", fill = "Inflow",
                 title = "Annual average lake loading") +
    ggplot2::theme_bw()
}
