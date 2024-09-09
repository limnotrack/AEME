#' Plot phytoplankton variables
#'
#' @inheritParams build_aeme
#' @param depth_range numeric; range of depths to plot. Default is NULL, which
#' averages over all depths.
#'
#' @return A ggplot object
#' @export
#'

plot_zoops <- function(aeme, model, add_obs = TRUE, depth_range = NULL,
                       ens_n = 1) {

  zoop_vars <- model_controls |>
    dplyr::filter(grepl("ZOO", var_aeme) & simulate) |>
    dplyr::pull(var_aeme)


  p1 <- plot_ts(aeme = aeme, model = model, var_sim = zoop_vars,
                ens_n = ens_n, depth_range = depth_range, add_obs = add_obs)
  return(p1)
}
