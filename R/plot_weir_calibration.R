#' Visualise calibrated weir parameters (C, h_inv) from calc_water_balance().
#'
#' @inheritParams build_aeme
#'
#' @return A patchwork object.
#' @export
plot_weir_calibration <- function(aeme, model) {
  
  aeme <- check_aeme(aeme)
  if (missing(model)) {
    model <- list_models(aeme)
  }
  
  if (length(model) > 1) {
    cli::cli_abort("Please specify a single model for water balance plotting.")
  }
  
  wbal <- aeme |>
    water_balance()
  
  outf <- aeme |>
    outflows()
  
  obs      <- aeme |> observations()
  obs_wlev <- obs[["level"]]
  
  if (length(outf[["data"]][["outflow"]]) > 0) {
    obs_out <- outf[["data"]][["outflow"]] |>
      dplyr::rename(obs_O = outflow)
  } else {
    obs_out <- NULL
  }
  
  if (length(wbal[["data"]][["wbal"]]) == 0) {
    cli::cli_alert_warning("No water balance data available.")
    return(NULL)
  }
  
  wb <- wbal[["data"]][["wbal"]] |>
    dplyr::filter(model == !!model)
  
  params <- wbal[["params"]]
  C      <- params["C"]
  h_inv  <- params["h_inv"]
  
  # Join observed water level onto wb for residual panels
  if (!is.null(obs_wlev) && nrow(obs_wlev) > 0) {
    wb <- wb |>
      dplyr::left_join(
        dplyr::select(obs_wlev, Date, lvl_obs = value),
        by = "Date"
      )
  }
  
  has_obs <- "lvl_obs" %in% names(wb) && any(!is.na(wb$lvl_obs))
  
  # Join observed outflow onto wb for rating curve overlay
  has_obs_out <- !is.null(obs_out) && nrow(obs_out) > 0
  if (has_obs_out) {
    wb <- wb |>
      dplyr::left_join(obs_out, by = "Date")
  }
  
  # Derived columns
  wb <- wb |>
    dplyr::mutate(
      # Reconstruct weir outflow from calibrated params + simulated level
      # (mirrors simulate_lake_nudged step 2 exactly)
      weir_Q_check = .weir_Q(value, C, h_inv),
      # Active head above invert
      head          = pmax(value - h_inv, 0),
      # Mass-balance residual: dV - net_flux
      # net_flux = inflow + rain - evap - HYD_outflow - spill_outflow
      net_flux      = HYD_flow + rain - evap_m3 - HYD_outflow - spill_outflow,
      mb_residual   = deltaV - net_flux
    )
  
  # Colours
  col_sim    <- "#2166ac"
  col_obs    <- "#d73027"
  col_level  <- "#4dac26"
  col_inv    <- "#762a83"
  col_dead   <- "#f0e6f7"
  col_head   <- "#abd9e9"
  col_resid  <- "#f46d43"
  col_weir   <- "#e08214"
  
  subtitle <- paste0(
    "Model: ", model, "  |  ",
    "C = ", signif(C, 4), "  |  h_inv = ", round(h_inv, 3), " masl"
  )
  
  base_theme <- ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin      = ggplot2::margin(4, 8, 4, 8)
    )
  
  # ── Panel 1 · Rating curve: Q vs h ────────────────────────────────────────
  # Shows the shape of the weir equation across the observed level range.
  # A bad h_inv shifts the curve left/right; a bad C scales it up/down.
  h_range  <- range(wb$value, na.rm = TRUE)
  h_seq    <- seq(h_range[1], h_range[2], length.out = 400)
  curve_df <- data.frame(h = h_seq, Q = .weir_Q(h_seq, C, h_inv))
  
  p1 <- ggplot2::ggplot() +
    ggplot2::annotate("rect",
                      xmin = -Inf, xmax = h_inv,
                      ymin = -Inf, ymax = Inf,
                      fill = col_dead, alpha = 0.5
    ) +
    ggplot2::geom_vline(xintercept = h_inv, colour = col_inv,
                        linetype = "dashed", linewidth = 0.7) +
    # Observed outflow-level scatter (spill_outflow vs simulated level)
    ggplot2::geom_point(
      data    = wb,
      mapping = ggplot2::aes(x = value, y = spill_outflow,
                             colour = "Simulated outflow\n(spill_outflow)"),
      alpha = 0.35, size = 1.2
    ) +
    # Observed outflow points if available (obs_O vs observed level)
    { if (has_obs_out && has_obs)
      ggplot2::geom_point(
        data    = dplyr::filter(wb, !is.na(obs_O), !is.na(lvl_obs)),
        mapping = ggplot2::aes(x = lvl_obs, y = obs_O,
                               colour = "Observed outflow"),
        alpha = 0.6, size = 1.5, shape = 17
      )
    } +
    ggplot2::geom_line(
      data    = curve_df,
      mapping = ggplot2::aes(x = h, y = Q, colour = "Weir curve\nC\u00b7max(h-h_inv,0)^1.5\u00b786400"),
      linewidth = 1
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        "Simulated outflow\n(spill_outflow)"              = col_sim,
        "Observed outflow"                                = col_obs,
        "Weir curve\nC\u00b7max(h-h_inv,0)^1.5\u00b786400" = col_weir
      )
    ) +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::annotate("text", x = h_inv, y = max(curve_df$Q, na.rm = TRUE),
                      label = paste0("h_inv\n", round(h_inv, 3)),
                      hjust = -0.1, vjust = 1, colour = col_inv, size = 3) +
    ggplot2::labs(
      x      = "Simulated lake level (masl)",
      y      = expression("Outflow (m"^3*" day"^-1*")"),
      colour = NULL,
      title  = "Rating curve"
    ) +
    base_theme +
    ggplot2::theme(legend.position = "bottom")
  
  # ── Panel 2 · Lake level time series: sim vs obs + h_inv reference ─────────
  # Shows where the lake sits relative to h_inv (shaded head = active weir).
  # If obs available, disagreement between sim and obs reveals nudging quality.
  p2 <- ggplot2::ggplot(wb, ggplot2::aes(x = Date)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = h_inv, ymax = pmax(value, h_inv),
                   fill = "Active head"),
      alpha = 0.3
    ) +
    ggplot2::geom_line(ggplot2::aes(y = value, colour = "Simulated"),
                       linewidth = 0.7) +
    { if (has_obs)
      ggplot2::geom_point(
        ggplot2::aes(y = lvl_obs, colour = "Observed"),
        size = 1.5, alpha = 0.7, na.rm = TRUE
      )
    } +
    ggplot2::geom_hline(yintercept = h_inv, colour = col_inv,
                        linetype = "dashed", linewidth = 0.6) +
    ggplot2::annotate("text", x = min(wb$Date), y = h_inv,
                      label = paste0("h_inv = ", round(h_inv, 3)),
                      hjust = 0, vjust = -0.4, colour = col_inv, size = 3) +
    ggplot2::scale_colour_manual(
      values = c("Simulated" = col_sim, "Observed" = col_obs)
    ) +
    ggplot2::scale_fill_manual(values = c("Active head" = col_head)) +
    ggplot2::labs(
      x      = "Date",
      y      = "Lake level (masl)",
      colour = NULL, fill = NULL,
      title  = "Lake level: simulated vs observed"
    ) +
    base_theme +
    ggplot2::theme(legend.position = "bottom")
  
  # ── Panel 3 · Weir outflow time series ────────────────────────────────────
  # spill_outflow comes from simulate_lake_nudged() using calibrated C & h_inv.
  # weir_Q_check reconstructs it analytically — they should overlap exactly,
  # confirming the calibrated params are being applied correctly.
  # obs_O (from outflows()) is overlaid if available for direct comparison.
  p3 <- ggplot2::ggplot(wb, ggplot2::aes(x = Date)) +
    ggplot2::geom_line(
      ggplot2::aes(y = spill_outflow, colour = "spill_outflow\n(from simulation)"),
      linewidth = 0.7
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = weir_Q_check, colour = "C\u00b7max(h-h_inv,0)^1.5\u00b786400\n(from calibrated params)"),
      linewidth = 0.5, linetype = "dashed"
    ) +
    { if (has_obs_out)
      ggplot2::geom_point(
        data    = dplyr::filter(wb, !is.na(obs_O)),
        mapping = ggplot2::aes(y = obs_O, colour = "Observed outflow"),
        size = 1.5, alpha = 0.7
      )
    } +
    ggplot2::scale_colour_manual(
      values = c(
        "spill_outflow\n(from simulation)"                         = col_sim,
        "C\u00b7max(h-h_inv,0)^1.5\u00b786400\n(from calibrated params)" = col_weir,
        "Observed outflow"                                         = col_obs
      )
    ) +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::labs(
      x      = "Date",
      y      = expression("Outflow (m"^3*" day"^-1*")"),
      colour = NULL,
      title  = "Weir outflow: simulation vs analytical check"
    ) +
    base_theme +
    ggplot2::theme(legend.position = "bottom")
  
  # ── Panel 4 · Mass-balance residual: dV - net_flux ────────────────────────
  # A zero residual means the water balance closes perfectly.
  # Systematic non-zero residual indicates a structural error:
  #   - Positive bias at high h → h_inv too high (over-estimating head)
  #   - C scales all residuals proportionally
  # This mirrors the `residual` vector tracked inside simulate_lake_nudged().
  p4 <- ggplot2::ggplot(wb, ggplot2::aes(x = Date)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey50",
                        linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(y = mb_residual), colour = col_resid,
                       linewidth = 0.6, alpha = 0.8) +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::labs(
      x     = "Date",
      y     = expression("Residual \u0394V - net flux (m"^3*")"),
      title = "Mass-balance residual  — zero = perfect closure"
    ) +
    base_theme
  
  # ── Panel 5 · Residual vs level (only if obs available) ───────────────────
  # Distinguishes C error (uniform scaling) from h_inv error (sign change
  # near the true invert). A loess trend crossing zero at the wrong level
  # is the clearest signal of a misplaced h_inv.
  if (has_obs) {
    wb_obs <- dplyr::filter(wb, !is.na(lvl_obs))
    wb_obs <- wb_obs |>
      dplyr::mutate(level_resid = value - lvl_obs)
    
    p5 <- ggplot2::ggplot(wb_obs, ggplot2::aes(x = lvl_obs, y = level_resid)) +
      ggplot2::geom_hline(yintercept = 0, colour = "grey50",
                          linetype = "dashed") +
      ggplot2::geom_vline(xintercept = h_inv, colour = col_inv,
                          linetype = "dashed", linewidth = 0.6) +
      ggplot2::geom_point(colour = col_resid, alpha = 0.5, size = 1.8) +
      ggplot2::geom_smooth(method = "loess", formula = y ~ x,
                           colour = "grey20", linewidth = 0.7,
                           se = TRUE, fill = "grey70", alpha = 0.25,
                           na.rm = TRUE) +
      ggplot2::annotate("text", x = h_inv, y = Inf,
                        label = paste0("h_inv\n", round(h_inv, 3)),
                        hjust = -0.1, vjust = 1.3, colour = col_inv, size = 3) +
      ggplot2::labs(
        x     = "Observed lake level (masl)",
        y     = "Level residual: sim - obs (m)",
        title = "Level residual vs observed level"
      ) +
      base_theme
    
    layout <- (p1 | p2) / (p3 | p5) / p4 +
      patchwork::plot_layout(heights = c(1, 1, 0.7))
  } else {
    layout <- (p1 | p2) / p3 / p4 +
      patchwork::plot_layout(heights = c(1, 0.8, 0.7))
  }
  
  layout +
    patchwork::plot_annotation(
      title    = "Weir Calibration Diagnostics",
      subtitle = subtitle,
      theme    = ggplot2::theme(
        plot.title    = ggplot2::element_text(face = "bold", size = 14),
        plot.subtitle = ggplot2::element_text(colour = "grey40", size = 11)
      )
    )
}

#' Weir outflow (m3/day) as a function of lake level (h), calibrated weir coefficient (C),
#' and invert level (h_inv). This is the equation used inside simulate_lake_nudged() to
#' calculate spill_outflow at each time step, and is the basis of the rating curve in panel 1.
#' 
#' @param h      numeric vector; simulated lake level (masl).
#' @param C      numeric; calibrated weir coefficient.
#' @param h_inv  numeric; calibrated weir invert level (masl).
#' @noRd
.weir_Q <- function(h, C, h_inv) {
  C * pmax(h - h_inv, 0)^1.5 * 86400
}