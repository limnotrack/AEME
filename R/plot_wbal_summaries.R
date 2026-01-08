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
  
  model_cols <- c("DYRESM-CAEDYM" = "#1B9E77", "GLM-AED" = "#D95F02",
                  "GOTM-WET" = "#7570B3", "Est." = "#E6AB02") 
  
  ## --- Lake level ---
  p1 <- ggplot() +
    geom_line(data = mod$level, aes(Date, value, color = Model)) +
    geom_line(data = wb, aes(Date, level, colour = "Est.")) +
    geom_point(data = obs, aes(Date, level, colour = "Obs"), size = 0.8) +
    scale_color_manual(values = c("Obs" = "red", model_cols),
                       name = "") +
    labs(title = "Lake Level", y = "Level (m)", x = "Date") +
    theme_bw()
  
  ## --- Total inflow ---
  p2 <- ggplot() +
    geom_col(data = mod_sum$inflow,
             aes(Model, value, fill = Model)) +
    geom_hline(data = wb_sum,
               aes(yintercept = inflow, colour = "Est."),
               linetype = "dashed") +
    scale_color_manual(values = model_cols, name = "") +
    scale_fill_manual(values = model_cols, name = "Model") +
    labs(title = "Total Inflow", y = "m³") +
    theme_bw()
  
  ## --- Total outflow ---
  p3 <- ggplot() +
    geom_col(data = mod_sum$outflow,
             aes(Model, value, fill = Model)) +
    geom_hline(data = wb_sum,
               aes(yintercept = outflow, colour = "Est."),
               linetype = "dashed") +
    scale_color_manual(values = model_cols, name = "") +
    scale_fill_manual(values = model_cols, name = "Model") +
    labs(title = "Total Outflow", y = "m³") +
    theme_bw()
  
  ## --- Total rainfall ---
  p4 <- ggplot() +
    geom_col(data = mod_sum$rain,
             aes(Model, value, fill = Model)) +
    geom_hline(data = wb_sum,
               aes(yintercept = rain, colour = "Est."),
               linetype = "dashed") +
    scale_color_manual(values = model_cols, name = "") +
    scale_fill_manual(values = model_cols, name = "Model") +
    labs(title = "Total Rainfall", y = "m³") +
    theme_bw()
  
  ## --- Total evaporation ---
  p5 <- ggplot() +
    geom_col(data = mod_sum$evap,
             aes(Model, value, fill = Model)) +
    geom_hline(data = wb_sum,
               aes(yintercept = evap_m3, colour = "Est."),
               linetype = "dashed") +
    scale_color_manual(values = model_cols, name = "") +
    scale_fill_manual(values = model_cols, name = "Model") +
    labs(title = "Total Evaporation", y = "m³") +
    theme_bw()
  
  patchwork::wrap_plots(
    p1, p2, p3, p4, p5,
    design = "
    AAAA
    BCDE",
    guides = "collect"
  )
}
