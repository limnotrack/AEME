#' Plot Water Balance Comparison
#'
#' @inheritParams plot_wbal_summaries
#'
#' @returns ggplot object
#' @export
#'
plot_wbal_comp <- function(aeme) {
  
  wbal <- get_wbal_components(aeme)
  
  wb      <- wbal$wb
  mod     <- wbal$mod
  
  ## --- Lake level ---
  comp <-  wb |> 
    dplyr::select(Date, model, level) |>
    dplyr::rename(est = level) |>
    dplyr::left_join(mod$level, by = c("Date", "model")) |> 
    dplyr::mutate(Model = toggle_models(model))
  lims <- range(comp$est, comp$value, na.rm = TRUE)
  p1 <- ggplot2::ggplot(comp) +
    ggplot2::geom_point(ggplot2::aes(x = est, y = value)) +
    ggplot2::geom_smooth(ggplot2::aes(x = est, y = value), method = "lm", se = FALSE, color = "blue") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
    ggplot2::coord_equal(xlim = lims, ylim = lims) +
    ggplot2::facet_wrap(~Model) +
    ggplot2::labs(title = "Lake Level Comparison", x = "Estimated Level (m)", y = "Model Level (m)") +
    ggplot2::theme_bw()
  ## --- Total inflow ---
  comp <- wb |> 
    dplyr::select(Date, model, HYD_flow) |>
    dplyr::rename(est = HYD_flow) |>
    dplyr::left_join(mod$inflow, by = c("Date", "model")) |> 
    dplyr::mutate(Model = toggle_models(model)) |> 
    dplyr::filter(est > 0 & value > 0)
  
  p2 <- ggplot2::ggplot(comp) +
    ggplot2::geom_point(ggplot2::aes(x = est, y = value)) +
    ggplot2::geom_smooth(ggplot2::aes(x = est, y = value), method = "lm", se = FALSE, color = "blue") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
    ggplot2::coord_equal(xlim = range(comp$est, comp$value, na.rm = TRUE),
                         ylim = range(comp$est, comp$value, na.rm = TRUE)) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10() +
    ggplot2::facet_wrap(~Model) +
    ggplot2::labs(
      title = "Total Inflow Comparison",
      x     = "Estimated Inflow (m\u00b3)",
      y     = "Model Inflow (m\u00b3)"
    ) +
    ggplot2::theme_bw()
  
  ## --- Total outflow ---
  comp <- wb |> 
    dplyr::select(Date, model, spill_outflow) |>
    dplyr::rename(est = spill_outflow) |>
    dplyr::left_join(mod$outflow, by = c("Date", "model")) |> 
    dplyr::mutate(Model = toggle_models(model)) |> 
    dplyr::filter(est > 0 & value > 0)
  p3 <- ggplot2::ggplot(comp) +
    ggplot2::geom_point(ggplot2::aes(x = est, y = value)) +
    ggplot2::geom_smooth(ggplot2::aes(x = est, y = value), method = "lm", se = FALSE, color = "blue") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
    ggplot2::coord_equal(xlim = range(comp$est, comp$value, na.rm = TRUE),
                         ylim = range(comp$est, comp$value, na.rm = TRUE)) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10() +
    ggplot2::facet_wrap(~Model) +
    ggplot2::labs(
      title = "Total Outflow Comparison",
      x = "Estimated Outflow (m\u00b3)",
      y = "Model Outflow (m\u00b3)") +
    ggplot2::theme_bw()
  
  ## --- Total rainfall ---
  # comp <- wb |> 
  #   dplyr::select(Date, model, rain) |>
  #   dplyr::rename(est = rain) |>
  #   dplyr::left_join(mod$rain, by = c("Date", "model")) |> 
  #   dplyr::mutate(Model = toggle_models(model)) |> 
  #   dplyr::filter(est > 0 & value > 0)
  # p4 <- ggplot(comp) +
  #   geom_point(ggplot2::aes(x = est, y = value)) +
  #   geom_smooth(ggplot2::aes(x = est, y = value), method = "lm",
  #               se = FALSE, color = "blue") +
  #   geom_abline(slope = 1, intercept = 0, color = "red") +
  #   coord_equal(xlim = range(comp$est, comp$value, na.rm = TRUE),
  #               ylim = range(comp$est, comp$value, na.rm = TRUE)) +
  #   scale_x_log10() +
  #   scale_y_log10() +
  #   facet_wrap(~Model) +
  #   labs(title = "Total Rainfall Comparison", x = "Estimated Rainfall (m\u00b3)", y = "Model Rainfall (m\u00b3)") +
  #   theme_bw()
  
  
  evap_0 <- wb |> 
    dplyr::filter(evap_m3 <= 0)
  
  mod_evap_0 <- mod$evap |> 
    dplyr::filter(value <= 0)
  
  ## --- Total evaporation ---
  comp <- wb |> 
    dplyr::select(Date, model, evap_m3) |>
    dplyr::rename(est = evap_m3) |>
    dplyr::left_join(mod$evap, by = c("Date", "model")) |> 
    dplyr::mutate(Model = toggle_models(model)) |> 
    dplyr::filter(est > 0 & value > 0)
  
  p4 <- ggplot2::ggplot(comp) +
    ggplot2::geom_point(ggplot2::aes(x = est, y = value)) +
    ggplot2::geom_smooth(ggplot2::aes(x = est, y = value), method = "lm",
                         se = FALSE, color = "blue") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
    ggplot2::coord_equal(xlim = range(comp$est, comp$value, na.rm = TRUE),
                         ylim = range(comp$est, comp$value, na.rm = TRUE)) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10() +
    ggplot2::facet_wrap(~Model) +
    ggplot2::labs(title = "Total Evaporation Comparison", 
                  x = "Estimated Evaporation (m\u00b3)",
                  y = "Model Evaporation (m\u00b3)") +
    ggplot2::theme_bw()
  
  ## --- Surface temperature ---
  comp <- wb |> 
    dplyr::select(Date, model, Ts) |>
    dplyr::rename(est = Ts) |>
    dplyr::left_join(mod$ts, by = c("Date", "model")) |> 
    dplyr::mutate(Model = toggle_models(model))
  
  p5 <- ggplot2::ggplot(comp) +
    ggplot2::geom_point(ggplot2::aes(x = est, y = value)) +
    ggplot2::geom_smooth(ggplot2::aes(x = est, y = value), method = "lm", se = FALSE, color = "blue") +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
    ggplot2::coord_equal(xlim = range(comp$est, comp$value, na.rm = TRUE),
                         ylim = range(comp$est, comp$value, na.rm = TRUE)) +
    ggplot2::facet_wrap(~Model) +
    ggplot2::labs(
      title = "Surface Temperature Comparison", 
      x = "Estimated Surface Temp (\u00b0C)", 
      y = "Model Surface Temp (\u00b0C)") +
    ggplot2::theme_bw()
  mae <- mean(abs(comp$est - comp$value), na.rm = TRUE)
  
  patchwork::wrap_plots(p2, p3, p4, p5, ncol = 2)
}
