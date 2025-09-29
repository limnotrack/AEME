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

plot_phs <- function(aeme, model, add_obs = TRUE, depth_range = NULL,
                     remove_spin_up = TRUE, ens_n = 1) {

  # Set colours for Nitrogen variables
  phs_cols <- c("Particulate inorganic P" = "#0072B2",
                "Phosphate" = "#D55E00",
                "Dissolved organic P" = "#CC79A7",
                "Particulate organic P" = "#009E73",
                "Total phosphorus" = "#F0E442")
  phs_lwd <- c(1, 1, 1, 1, 2)
  phs_alpha <- c(1, 1, 1, 1, 0.5)
  names(phs_lwd) <- names(phs_cols)
  names(phs_alpha) <- names(phs_cols)


  phs_vars <- model_controls |>
    dplyr::filter(grepl("PHS", var_aeme) & simulate) |>
    dplyr::pull(var_aeme)

  y_lab <- eval(parse(text = "P~(g~m^-3)"))

  p1 <- plot_ts(aeme = aeme, model = model, var_sim = phs_vars,
                remove_spin_up = remove_spin_up, ens_n = ens_n, 
                depth_range = depth_range, add_obs = add_obs) +
    ggplot2::labs(x = "Date", y = y_lab, colour = "Group") +
    ggplot2::guides(linewidth = "none", alpha = "none")

  p1$scales$scales <- list()

  p1 <- p1 +
    ggplot2::scale_color_manual(values = phs_cols) +
    ggplot2::scale_linewidth_manual(values = phs_lwd) +
    ggplot2::scale_alpha_manual(values = phs_alpha)

  return(p1)
}
