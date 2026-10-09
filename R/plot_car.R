#' Plot carbon variables
#'
#' @inheritParams plot_output
#' @inheritParams get_var
#' @param depth_range numeric; range of depths to plot. Default is NULL, which
#' averages over all depths.
#'
#' @return A ggplot object
#' @export
#'

plot_car <- function(aeme, model, add_obs = TRUE, depth_range = NULL,
                     remove_spin_up = TRUE, ens_n = 1) {
  
  aeme <- check_aeme(aeme)
  if (missing(model)) {
    model <- list_models(aeme)
  } else {
    model <- check_model(model = model)
  }
  # Set colours for Nitrogen variables
  car_cols <- c("Dissolved inorganic C" = "#2166AC",
                "Dissolved organic C" = "#8C510A",
                "Particulate organic C" = "#4DAC26",
                "Carbon CH4" = "#762A83",
                "Total organic C" = "#D6604D"
                )
  car_lwd <- c(1, 1, 1, 1, 2)
  car_alpha <- c(1, 1, 1, 1, 0.5)
  names(car_lwd) <- names(car_cols)
  names(car_alpha) <- names(car_cols)
  
  
  car_vars <- model_controls |>
    dplyr::filter(grepl("CAR", var_aeme) & simulate) |>
    dplyr::pull(var_aeme)
  
  y_lab <- eval(parse(text = "C~(g~m^-3)"))
  
  p1 <- plot_ts(aeme = aeme, model = model, var_sim = car_vars,
                remove_spin_up = remove_spin_up, ens_n = ens_n, 
                depth_range = depth_range, add_obs = add_obs) +
    ggplot2::labs(x = "Date", y = y_lab, colour = "Group") +
    ggplot2::guides(linewidth = "none", alpha = "none")
  
  p1$scales$scales <- list()
  
  p1 <- p1 +
    ggplot2::scale_color_manual(values = car_cols) +
    ggplot2::scale_linewidth_manual(values = car_lwd) +
    ggplot2::scale_alpha_manual(values = car_alpha)
  
  return(p1)
}
