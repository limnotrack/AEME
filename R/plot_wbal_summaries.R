#' Plot Water Balance Summaries
#'
#' @param wbal Water balance object created with `get_wbal_components()`
#'
#' @returns A ggplot2 object with water balance summaries
#' @export
#'
plot_wbal_summaries <- function(wbal) {
  
  wb      <- wbal$wb
  wb_sum  <- wbal$wb_sum
  obs     <- wbal$obs
  mod     <- wbal$mod
  mod_sum <- wbal$mod_sum
  
  
  model_cols <- c("DYRESM-CAEDYM" = "#1B9E77", "GLM-AED" = "#66A61E",
                  "GOTM-WET" = "#7570B3", "Est." = "#E6AB02") 
  
  ## --- Lake level ---
  wb_lev <- wb |> 
    dplyr::group_by(Date) |>
    dplyr::summarise(level = mean(level, na.rm = TRUE))
  p1 <- ggplot2::ggplot() +
    ggplot2::geom_line(data = mod$level, ggplot2::aes(Date, value, color = Model)) +
    ggplot2::geom_line(data = wb_lev, ggplot2::aes(Date, level, colour = "Est.")) +
    ggplot2::scale_color_manual(values = c("Obs" = "red", model_cols),
                       name = "") +
    ggplot2::labs(title = "Lake Level", y = "Level (m)", x = "Date") +
    ggplot2::theme_bw()
  
  if (!is.null(obs$level)) {
    p1 <- p1 +
      ggplot2::geom_point(data = obs, ggplot2::aes(Date, level, colour = "Obs"),
                          size = 0.8)
  }
  
  ## --- Total inflow ---
  p2 <- ggplot2::ggplot() +
    ggplot2::geom_col(data = mod_sum$inflow,
             ggplot2::aes(Model, value, fill = Model)) +
    ggplot2::geom_point(data = wb_sum, 
                        ggplot2::aes(x = Model, y = inflow, colour = "Est."),
               size = 5, shape = 3) +
    ggplot2::scale_color_manual(values = model_cols, name = "") +
    ggplot2::scale_fill_manual(values = model_cols, name = "Model") +
    ggplot2::labs(title = "Total Inflow", y = "m\u00b3") +
    ggplot2::theme_bw()
  
  ## --- Total outflow ---
  p3 <- ggplot() +
    ggplot2::geom_col(data = mod_sum$outflow,
             ggplot2::aes(Model, value, fill = Model)) +
    ggplot2::geom_point(data = wb_sum, 
                        ggplot2::aes(x = Model, y = outflow, colour = "Est."),
                        size = 5, shape = 3) +
    ggplot2::scale_color_manual(values = model_cols, name = "") +
    ggplot2::scale_fill_manual(values = model_cols, name = "Model") +
    ggplot2::labs(title = "Total Outflow", y = "m\u00b3") +
    ggplot2::theme_bw()
  
  ## --- Total rainfall ---
  p4 <- ggplot() +
    ggplot2::geom_col(data = mod_sum$rain,
             ggplot2::aes(Model, value, fill = Model)) +
    ggplot2::geom_point(data = wb_sum, 
                        ggplot2::aes(x = Model, y = rain, colour = "Est."),
                        size = 5, shape = 3) +
    ggplot2::scale_color_manual(values = model_cols, name = "") +
    ggplot2::scale_fill_manual(values = model_cols, name = "Model") +
    ggplot2::labs(title = "Total Rainfall", y = "m\u00b3") +
    ggplot2::theme_bw()
  
  ## --- Total evaporation ---
  p5 <- ggplot() +
    ggplot2::geom_col(data = mod_sum$evap,
             ggplot2::aes(Model, value, fill = Model)) +
    ggplot2::geom_point(data = wb_sum, 
                        ggplot2::aes(x = Model, y = evap_m3, colour = "Est."),
                        size = 5, shape = 3) +
    ggplot2::scale_color_manual(values = model_cols, name = "") +
    ggplot2::scale_fill_manual(values = model_cols, name = "Model") +
    ggplot2::labs(title = "Total Evaporation", y = "m\u00b3") +
    ggplot2::theme_bw()
  
  patchwork::wrap_plots(
    p1, p2, p3, p4, p5,
    design = "
    AAAA
    BCDE",
    guides = "collect"
  )
}
