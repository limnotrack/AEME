#' Visualise calibrated weir parameters (C, h_inv) from calc_water_balance().
#'
#' @inheritParams build_aeme
#' @param model character; model name(s) to plot. Multiple models are
#'   overlaid on the same panels, coloured by model, rather than faceted --
#'   useful for comparing evaporation families (e.g. `glm_aed` vs
#'   `gotm_wet`) or confirming that `dy_cd`/`glm_aed` share a fit. Defaults
#'   to every model present in `aeme`.
#'
#' @import ggplot2
#' @importFrom scales label_comma
#' @importFrom patchwork plot_layout plot_annotation
#'
#' @return A patchwork object.
#' @export
plot_weir_calibration <- function(aeme, model) {

  aeme <- check_aeme(aeme)
  if (missing(model)) {
    model <- list_models(aeme)
  }
  model <- check_model(model = model)
  model_disp <- names(model)
  model <- unname(model)

  wbal <- aeme |>
    water_balance()

  outf <- aeme |>
    outflows()

  obs      <- aeme |> observations()
  obs_wlev <- obs[["level"]]

  if (length(outf[["data"]][["outflow"]]) > 0) {
    obs_out <- outf[["data"]][["outflow"]] |>
      dplyr::rename(obs_O = HYD_flow)
  } else {
    obs_out <- NULL
  }

  if (length(wbal[["data"]][["wbal"]]) == 0) {
    cli::cli_alert_warning("No water balance data available.")
    return(NULL)
  }

  wb <- wbal[["data"]][["wbal"]] |>
    dplyr::filter(model %in% !!model)

  # ---- Per-model fitted parameters (may differ by evaporation family) ----
  params_df <- lapply(model, \(m) {
    p <- resolve_wbal_params(wbal[["params"]], wbal_evap_family(m))
    data.frame(model = m, C = if (is.null(p)) NA_real_ else unname(p["C"]),
              h_inv  = if (is.null(p)) NA_real_ else unname(p["h_inv"]))
  }) |>
    dplyr::bind_rows()

  missing_fit <- params_df$model[is.na(params_df$C)]
  if (length(missing_fit) > 0) {
    cli::cli_abort("No fitted water balance parameters found for {.val {missing_fit}}.")
  }

  model_levels <- setNames(model_disp, model)
  wb <- wb |>
    dplyr::left_join(params_df, by = "model") |>
    dplyr::mutate(model_label = factor(model_levels[model], levels = model_disp))
  params_df <- params_df |>
    dplyr::mutate(model_label = factor(model_levels[model], levels = model_disp))

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

  # Derived columns -- C/h_inv now vary per row via the model-specific join
  # above, so these are per-model even when multiple models are plotted.
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

  # Colours (model-agnostic overlays only; per-model series use the default
  # ggplot categorical scale via `model_label`, kept consistent across
  # panels by the shared factor levels set above)
  col_obs_pt <- "grey30"

  subtitle <- paste0(
    "Model", if (nrow(params_df) > 1) "s" else "", ": ",
    paste0(params_df$model_label, " (C=", signif(params_df$C, 3),
          ", h_inv=", round(params_df$h_inv, 3), ")", collapse = "  |  ")
  )

  base_theme <- ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin      = ggplot2::margin(4, 8, 4, 8)
    )

  # Panel 1  Rating curve: Q vs h
  # Shows the shape of the weir equation across the observed level range,
  # one curve per model (colour). A bad h_inv shifts a model's curve
  # left/right; a bad C scales it up/down.
  h_range  <- range(wb$value, na.rm = TRUE)
  h_seq    <- seq(h_range[1], h_range[2], length.out = 400)
  curve_df <- lapply(seq_len(nrow(params_df)), \(i) {
    data.frame(model_label = unname(params_df$model_label[i]), h = h_seq,
              Q = .weir_Q(h_seq, params_df$C[i], params_df$h_inv[i]))
  }) |>
    dplyr::bind_rows()
  
  # Define once, outside all plots
  model_levels <- levels(wb$model_label)  # c("GLM-AED", "GOTM-WET", "SIMSTRAT-AED2")
  shared_colour <- ggplot2::scale_colour_discrete(
    name   = "Model",
    breaks = model_levels,
    limits = model_levels
  )

  p1 <- ggplot2::ggplot() +
    ggplot2::geom_vline(
      data    = params_df,
      mapping = ggplot2::aes(xintercept = h_inv, colour = model_label),
      linetype = "dashed", linewidth = 0.6, show.legend = FALSE
    ) +
    # Simulated outflow-level scatter (spill_outflow vs simulated level)
    ggplot2::geom_point(
      data    = wb,
      mapping = ggplot2::aes(x = value, y = spill_outflow, colour = model_label),
      alpha = 0.3, size = 1, show.legend = FALSE
    ) +
    # Observed outflow points if available (obs_O vs observed level) --
    # model-agnostic, so plotted once in a neutral colour
    { if (has_obs_out && has_obs)
      ggplot2::geom_point(
        data    = dplyr::filter(wb, !is.na(obs_O), !is.na(lvl_obs)) |>
          dplyr::distinct(Date, .keep_all = TRUE),
        mapping = ggplot2::aes(x = lvl_obs, y = obs_O),
        colour = col_obs_pt, alpha = 0.6, size = 1.5, shape = 17, 
        show.legend = FALSE
      )
    } +
    ggplot2::geom_line(
      data    = curve_df,
      mapping = ggplot2::aes(x = h, y = Q, colour = model_label),
      linewidth = 1
    ) +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::labs(
      x      = "Simulated lake level (masl)",
      y      = expression("Outflow (m"^3*" day"^-1*")"),
      colour = "Model",
      title  = "Rating curve",
      caption = if (has_obs_out) "Grey triangles: observed outflow; dashed lines: h_inv" else "Dashed lines: h_inv"
    ) +
    base_theme +
    shared_colour

  #  Panel 2  Lake level time series: sim vs obs, one line per model + h_inv reference
  # Shows where each model sits relative to its own h_inv.
  # If obs available, disagreement between sim and obs reveals nudging quality.
  p2 <- ggplot2::ggplot(wb, ggplot2::aes(x = Date)) +
    ggplot2::geom_hline(
      data    = params_df,
      mapping = ggplot2::aes(yintercept = h_inv, colour = model_label),
      linetype = "dashed", linewidth = 0.5, show.legend = FALSE
    ) +
    ggplot2::geom_line(ggplot2::aes(y = value, colour = model_label),
                       linewidth = 0.7) +
    { if (has_obs)
      ggplot2::geom_point(
        data    = dplyr::distinct(wb, Date, lvl_obs),
        mapping = ggplot2::aes(y = lvl_obs),
        colour = col_obs_pt, size = 1.5, alpha = 0.7, na.rm = TRUE,
        show.legend = FALSE
      )
    } +
    ggplot2::labs(
      x      = "Date",
      y      = "Lake level (masl)",
      colour = "Model",
      title  = "Lake level: simulated vs observed"
    ) +
    base_theme +
    shared_colour

  #  Panel 3  Weir outflow time series
  # spill_outflow comes from simulate_lake_nudged() using each model's
  # calibrated C & h_inv. weir_Q_check reconstructs it analytically - they
  # should overlap exactly, confirming the calibrated params are applied
  # correctly. obs_O (from outflows()) is overlaid if available.
  p3 <- ggplot2::ggplot(wb, ggplot2::aes(x = Date)) +
    ggplot2::geom_line(
      ggplot2::aes(y = spill_outflow, colour = model_label),
      linewidth = 0.7
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = weir_Q_check, colour = model_label),
      linewidth = 0.5, linetype = "dashed"
    ) +
    { if (has_obs_out)
      ggplot2::geom_point(
        data    = dplyr::filter(wb, !is.na(obs_O)) |> dplyr::distinct(Date, obs_O),
        mapping = ggplot2::aes(y = obs_O),
        colour = col_obs_pt, size = 1.5, alpha = 0.7, shape = 17,
        show.legend = FALSE
      )
    } +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::labs(
      x      = "Date",
      y      = expression("Outflow (m"^3*" day"^-1*")"),
      colour = "Model",
      title  = "Weir outflow: simulation (solid) vs analytical check (dashed)"
    ) +
    base_theme +
    shared_colour

  #  Panel 4  Mass-balance residual: dV - net_flux
  # A zero residual means the water balance closes perfectly.
  # This mirrors the `residual` vector tracked inside simulate_lake_nudged().
  p4 <- ggplot2::ggplot(wb, ggplot2::aes(x = Date)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey50",
                        linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(y = mb_residual, colour = model_label),
                       linewidth = 0.6, alpha = 0.8) +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::labs(
      x     = "Date",
      y     = expression("Residual ΔV - net flux (m"^3*")"),
      colour = "Model",
      title = "Mass-balance residual  - zero = perfect closure"
    ) +
    base_theme +
    shared_colour

  #  Panel 5  Residual vs level (only if obs available)
  # Distinguishes C error (uniform scaling) from h_inv error (sign change
  # near the true invert), per model.
  if (has_obs) {
    wb_obs <- dplyr::filter(wb, !is.na(lvl_obs))
    wb_obs <- wb_obs |>
      dplyr::mutate(level_resid = value - lvl_obs)

    p5 <- ggplot2::ggplot(wb_obs, ggplot2::aes(x = lvl_obs, y = level_resid,
                                               colour = model_label)) +
      ggplot2::geom_hline(yintercept = 0, colour = "grey50",
                          linetype = "dashed") +
      ggplot2::geom_vline(
        data    = params_df,
        mapping = ggplot2::aes(xintercept = h_inv, colour = model_label),
        linetype = "dashed", linewidth = 0.5, show.legend = FALSE
      ) +
      ggplot2::geom_point(alpha = 0.4, size = 1.6, show.legend = FALSE) +
      ggplot2::geom_smooth(method = "loess", formula = y ~ x,
                           linewidth = 0.7, se = TRUE, alpha = 0.15,
                           na.rm = TRUE) +
      ggplot2::labs(
        x     = "Observed lake level (masl)",
        y     = "Level residual: sim - obs (m)",
        colour = "Model",
        title = "Level residual vs observed level"
      ) +
      base_theme +
      shared_colour
    
    patchwork::wrap_plots(p3, p4, ncol = 2, guides = "collect")

    layout <- ((p1 | p2) / (p3 | p5) / p4) +
      patchwork::plot_layout(heights = c(1, 1, 0.7), guides = "collect")
  } else {
    layout <- ((p1 | p2) / p3 / p4) +
      patchwork::plot_layout(heights = c(1, 0.8, 0.7), guides = "collect")
  }

  layout <- layout & ggplot2::theme(legend.position = "bottom")

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
