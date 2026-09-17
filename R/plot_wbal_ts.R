#' Plot Water Balance Components as Timeseries
#'
#' @inheritParams plot_wbal_summaries
#' @param var_aeme character vector of AEME variable names to plot. Options
#'   are \code{"LKE_lvlwtr"}, \code{"LKE_inflow"}, \code{"LKE_outflow"},
#'   \code{"LKE_pcpvol"}, \code{"LKE_Qe"}, \code{"HYD_surft"}. Defaults to
#'   all.
#' @param add_model logical; whether to overlay model output on the estimated
#'   components. Defaults to TRUE.
#'   
#' @importFrom ggplot2 ggplot aes geom_line labs theme_bw theme element_blank 
#' @importFrom ggplot2 scale_colour_brewer facet_wrap
#' @importFrom patchwork wrap_plots plot_layout
#'
#' @returns ggplot object
#' @export
#'
plot_wbal_ts <- function(aeme,
                         var_aeme = c("LKE_lvlwtr", "LKE_inflow", "LKE_outflow",
                                      "LKE_pcpvol", "LKE_Qe", "HYD_surft"),
                         add_model = TRUE) {
  
  # var_aeme <- match.arg(var_aeme, several.ok = TRUE)
  var_aeme <- guess_aeme_vars(var_aeme)
  
  wbal <- get_wbal_components(aeme)
  wb   <- wbal$wb |> 
    dplyr::mutate(Model = toggle_models(model))
  mod  <- wbal$mod
  
  n_model <- length(unique(wb$Model))
  
  # Helper to optionally add model layer
  add_model_layer <- function(p, mod_df) {
    if (add_model) {
      p <- p +
        ggplot2::geom_line(
          data = mod_df |> dplyr::mutate(Model = toggle_models(model)),
          ggplot2::aes(x = Date, y = value, colour = Model, group = Model),
          linewidth = 0.6, alpha = 0.6
        ) +
        ggplot2::scale_colour_brewer(palette = "Set1", name = "Model")
    }
    p
  }
  
  var_config <- list(
    LKE_lvlwtr = list(
      wb_col = "level",
      mod_df = mod$level,
      title  = "Lake Level",
      ylab   = "Level (m)"
    ),
    LKE_inflow = list(
      wb_col = "inflow",
      mod_df = mod$inflow,
      title  = "Total Inflow",
      ylab   = "Inflow (m\u00b3)"
    ),
    LKE_outflow = list(
      wb_col = "outflow",  # derived below
      mod_df = mod$outflow,
      title  = "Total Outflow",
      ylab   = "Outflow (m\u00b3)"
    ),
    LKE_pcpvol = list(
      wb_col = "rain",
      mod_df = mod$rain,
      title  = "Total Precipitation",
      ylab   = "Precipitation (m\u00b3)"
    ), 
    LKE_Qe = list(
      wb_col = "evap_m3",
      mod_df = mod$evap,
      title  = "Total Evaporation",
      ylab   = "Evaporation (m\u00b3)"
    ),
    HYD_surft = list(
      wb_col = "Ts",
      mod_df = mod$ts,
      title  = "Surface Temperature",
      ylab   = "Surface Temp (\u00b0C)"
    )
  )
  
  wb <- wb |>
    dplyr::mutate(outflow = HYD_outflow + spill_outflow)
  n_plots <- length(var_aeme)
  plots <- lapply(seq_along(var_aeme), function(i) {
    v <- var_aeme[i]
    cfg <- var_config[[v]]
    p <- ggplot2::ggplot(wb, ggplot2::aes(x = Date, y = .data[[cfg$wb_col]],
                                          colour = "Est.")) +
      ggplot2::geom_line(linewidth = 0.6) +
      ggplot2::labs(title = cfg$title, x = NULL, y = cfg$ylab) +
      ggplot2::theme_bw()
    
    if (i != n_plots) {
      p <- p +
        ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                       axis.ticks.x = ggplot2::element_blank())
    }
    if (n_model > 1) {
      p <- p +
        ggplot2::facet_wrap(~Model, ncol = 1) +
        ggplot2::theme(legend.position = "none")
    }
    add_model_layer(p, cfg$mod_df)
  })
  
  patchwork::wrap_plots(plots, ncol = 1) +
    patchwork::plot_layout(axes = "collect")
}
