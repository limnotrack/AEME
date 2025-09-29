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

plot_nit <- function(aeme, model, add_obs = TRUE, depth_range = NULL,
                     remove_spin_up = TRUE, ens_n = 1) {

  # Set colours for Nitrogen variables
  nit_cols <- c("Ammoniacal nitrogen" = "#1b9e77",
                "Nitrate" = "#d95f02",
                "Dissolved organic N" = "#7570b3",
                "Particulate organic N" = "#e7298a",
                "Total nitrogen" = "#66a61e")
  nit_lwd <- c(1, 1, 1, 1, 2)
  nit_alpha <- c(1, 1, 1, 1, 0.5)
  names(nit_lwd) <- names(nit_cols)
  names(nit_alpha) <- names(nit_cols)


  nit_vars <- model_controls |>
    dplyr::filter(grepl("NIT", var_aeme) & simulate) |>
    dplyr::pull(var_aeme)

  y_lab <- eval(parse(text = "N~(g~m^-3)"))

  p1 <- plot_ts(aeme = aeme, model = model, var_sim = nit_vars,
                remove_spin_up = remove_spin_up, ens_n = ens_n, 
                depth_range = depth_range, add_obs = add_obs) +
    ggplot2::labs(x = "Date", y = y_lab, colour = "Group") +
    ggplot2::guides(linewidth = "none", alpha = "none")

  p1$scales$scales <- list()

  p1 <- p1 +
    ggplot2::scale_color_manual(values = nit_cols) +
    ggplot2::scale_linewidth_manual(values = nit_lwd) +
    ggplot2::scale_alpha_manual(values = nit_alpha)

  return(p1)
}
