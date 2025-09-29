#' Plot phytoplankton variables
#'
#' @inheritParams build_aeme
#' @inheritParams get_var
#' @param depth_range numeric; range of depths to plot. Default is NULL, which
#' averages over all depths.
#'
#' @return A ggplot object
#' @export
#'

plot_zoops <- function(aeme, model, add_obs = TRUE, depth_range = NULL,
                       remove_spin_up = TRUE, ens_n = 1) {

  zoop_vars <- model_controls |>
    dplyr::filter(grepl("ZOO", var_aeme) & simulate) |>
    dplyr::pull(var_aeme)

  if (length(zoop_vars) == 0) {
    stop(strwrap("No zooplankton variables found in model controls.\nMake sure
                 to set simulate = TRUE in the `model_controls` for selected
                 variables when executing `run_aeme`.", width = 80))
  }


  p1 <- plot_ts(aeme = aeme, model = model, var_sim = zoop_vars,
                remove_spin_up = remove_spin_up, ens_n = ens_n,
                depth_range = depth_range, add_obs = add_obs)
  return(p1)
}
