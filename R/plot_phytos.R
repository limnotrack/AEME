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

plot_phytos <- function(aeme, model, add_obs = TRUE, remove_spin_up = TRUE,
                        depth_range = NULL, ens_n = 1) {

  # Set colours for phytopplankton variables
  phy_cols <- c("Cyanobacteria" = "#56B4E9",
                "Green algae" = "#009E73",
                "Cryptophytes" = "#0072B2",
                "Diatoms freshwater" = "#E69F00",
                "Total chlorophyll a" = "#00441B")
  phy_lwd <- c(1, 1, 1, 1, 2)
  phy_alpha <- c(1, 1, 1, 1, 0.3)
  names(phy_lwd) <- names(phy_cols)
  names(phy_alpha) <- names(phy_cols)


  phy_vars <- model_controls |>
    dplyr::filter(grepl("PHY", var_aeme) & simulate) |>
    dplyr::pull(var_aeme)

  y_lab <- eval(parse(text = "Chlorophyll~(mg~chla~m^-3)"))

  p1 <- plot_ts(aeme = aeme, model = model, var_sim = phy_vars,
                remove_spin_up = remove_spin_up, ens_n = ens_n, 
                depth_range = depth_range, add_obs = add_obs) +
    ggplot2::labs(x = "Date", y = y_lab, colour = "Group")

  p1$scales$scales <- list()

  p1 <- p1 +
    ggplot2::scale_color_manual(values = phy_cols) +
    ggplot2::scale_linewidth_manual(values = phy_lwd) +
    ggplot2::scale_alpha_manual(values = phy_alpha)
  return(p1)
}
