#' Plot a variable from a raw read_glm_output()/read_model_outputs() list
#'
#' A thin, backward-compatible alias for [plot_model_output()] -- kept for
#' existing callers. New code should call [plot_model_output()] directly,
#' which also accepts an `Aeme` object.
#'
#' @inheritParams plot_model_output
#' @param out list; as returned by [read_glm_output()] or
#'   [read_model_outputs()] (with `model = "glm_aed"`).
#'
#' @return A ggplot2 object -- see [plot_model_output()].
#' @export
#'
#' @examples
#' \dontrun{
#' out <- read_glm_output(file = outfile)
#' plot_glm_output(out, "HYD_temp")        # depth x time tile plot
#' plot_glm_output(out, "LKE_lvlwtr")      # simple time series
#' plot_glm_output(out, "SDF_Fsed_oxy_Z")  # one line per sediment zone
#' }
plot_glm_output <- function(out, var_sim, var_lims = NULL, ylim = NULL) {
  plot_model_output(out, var_sim = var_sim, var_lims = var_lims, ylim = ylim)
}
