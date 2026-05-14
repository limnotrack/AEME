#' Plot estimated water balance diagnostics from an aeme object.
#'
#' @param aeme      aeme object.
#' @param model     character; single model name. If missing, uses list_models().
#' @param time_axis one of \code{"auto"} (default), \code{"daily"},
#'                  \code{"monthly"}, or \code{"annual"}.
#'
#' @return A patchwork object, or NULL if no water balance data available.
#' @export
plot_est_wbal <- function(aeme, model,
                          time_axis = c("auto", "daily", "monthly", "annual")) {
  
  aeme <- check_aeme(aeme)
  if (missing(model)) {
    model <- list_models(aeme)
  }
  
  if (length(model) > 1) {
    cli::cli_abort("Please specify a single model for water balance plotting.")
  }
  
  wbal <- aeme |>
    water_balance()
  
  if (length(wbal[["data"]][["wbal"]]) == 0) {
    cli::cli_alert_warning("No water balance data available.")
    return(NULL)
  }
  
  wb <- wbal[["data"]][["wbal"]] |>
    dplyr::filter(model == !!model)
  
  plot_water_balance(wb = wb, time_axis = time_axis)
}

#' Helper function to aggregate water balance data based on the specified time axis.
#' Handles "daily", "monthly", "annual", and "auto" aggregation. For
#' "auto", selects resolution based on series length: <= 2 years -> daily, <= 6 years -> monthly, > 6 years -> annual.
#' Returns a list containing the aggregated data frame, x-axis label, resolved period, and
#' a logical indicating whether aggregation was performed (TRUE for monthly/annual, FALSE for daily).
#' Designed to be called from plot_water_balance() after subsetting to a single model.
#' @param df        data frame with daily water balance components (columns: Date, value, V, inflow, rain, evap_m3, HYD_outflow, spill_outflow, deltaV).
#' @param time_axis character; one of "daily", "monthly", "annual",
#' 
#' @noRd
.aggregate_wb <- function(df, time_axis) {
  
  # "auto" picks resolution based on series length:
  #   <= 2 years  -> daily
  #   <= 6 years  -> monthly
  #   >  6 years  -> annual
  resolved <- switch(
    time_axis,
    daily   = "daily",
    monthly = "monthly",
    annual  = "annual",
    auto    = {
      yrs <- as.numeric(diff(range(df$Date))) / 365.25
      if (yrs <= 2) "daily" else if (yrs <= 6) "monthly" else "annual"
    },
    stop("`time_axis` must be one of: \"daily\", \"monthly\", \"annual\", \"auto\"",
         call. = FALSE)
  )
  
  if (resolved == "daily") {
    return(list(data = df, x_lab = "Date",
                period = "daily", aggregated = FALSE))
  }
  
  # Build grouping date and labels
  # Annual bins run from the series start date, not calendar Jan 1
  start_date <- min(df$Date)
  df_agg <- if (resolved == "monthly") {
    df |>
      dplyr::mutate(period_date = as.Date(format(Date, "%Y-%m-01"))) |>
      dplyr::group_by(model, period_date)
  } else {
    df |>
      dplyr::mutate(
        elapsed_days = as.integer(Date - start_date),
        year_bin     = elapsed_days %/% 365L,
        period_date  = start_date + (year_bin * 365L)
      ) |>
      dplyr::group_by(model, period_date)
  }
  
  agg <- df_agg |>
    dplyr::summarise(
      value         = mean(value,        na.rm = TRUE),  # state var: mean
      V             = mean(V,            na.rm = TRUE),  # state var: mean
      inflow        = sum(inflow,        na.rm = TRUE),
      rain          = sum(rain,          na.rm = TRUE),
      evap_m3       = sum(evap_m3,       na.rm = TRUE),
      HYD_outflow   = sum(HYD_outflow,   na.rm = TRUE),
      spill_outflow = sum(spill_outflow, na.rm = TRUE),
      deltaV        = sum(deltaV,        na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::rename(Date = period_date)
  
  x_lab <- if (resolved == "monthly") "Date (monthly total)" else "Date (annual total)"
  
  list(data = agg, x_lab = x_lab, period = resolved, aggregated = TRUE)
}

# ── Helper: build one 4-panel plot for a single model ───────────────────────
#' Helper function to build a 4-panel water balance diagnostic plot for a single model.
#' Takes a data frame with daily or aggregated water balance components and constructs
#' a patchwork of four ggplot panels:
#' 1. Lake level (line plot)
#' 2. Lake volume (area plot)
#' 3. Fluxes (dodged + stacked bar chart of inflow, rain
#'   evaporation, outflow, spill)
#' 4. Delta V and net flux (line plot with two series)
#' The function handles both daily and aggregated data, adjusting x-axis scales, labels, and bar widths accordingly.
#' Designed to be called from plot_water_balance() after subsetting to a single model and aggregating as needed.
#' @param df          data frame with columns: Date, value (lake level), V (lake volume),
#'                   inflow, rain, evap_m3, HYD_outflow, spill_out
#'                   outflow, deltaV. Should contain data for a single model, either daily or aggregated.
#' @param model_name   character; name of the model (for plot subtitle).
#' @param x_lab        character; label for the x-axis of the bottom panel.
#' @param period       character; one of "daily", "monthly", or "annual
#' @param aggregated   logical; whether the data is aggregated (TRUE) or daily (FALSE). Affects plot labels and formatting.                   
#' @noRd
.build_model_plot <- function(df, model_name, x_lab, period, aggregated) {
  
  df <- df |>
    dplyr::mutate(
      outflow_total = HYD_outflow + spill_outflow,
      net_flux      = inflow + rain - evap_m3 - outflow_total
    )
  
  # Colour palette
  col_level   <- "#2166ac"
  col_volume  <- "#4dac26"
  col_inflow  <- "#1a9641"
  col_rain    <- "#74add1"
  col_evap    <- "#d73027"
  col_outflow <- "#f46d43"
  col_spill   <- "#fdae61"
  col_deltaV  <- "#762a83"
  col_net     <- "#1b7837"
  
  # Shared x scale — break density scales with resolution
  x_limits <- range(df$Date)
  x_scale <- switch(
    period,
    daily = ggplot2::scale_x_date(
      limits = x_limits,
      expand = ggplot2::expansion(mult = 0.01)
    ),
    monthly = ggplot2::scale_x_date(
      limits      = x_limits,
      date_breaks = "3 months",
      date_labels = "%b %Y",
      expand      = ggplot2::expansion(mult = 0.01)
    ),
    annual = ggplot2::scale_x_date(
      limits      = x_limits,
      date_breaks = "1 year",
      date_labels = "%Y",
      expand      = ggplot2::expansion(mult = 0.01)
    )
  )
  
  base_theme <- ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.title.x     = ggplot2::element_blank(),
      axis.text.x      = ggplot2::element_blank(),
      axis.ticks.x     = ggplot2::element_blank(),
      plot.margin      = ggplot2::margin(2, 8, 2, 8)
    )
  
  bottom_theme <- ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin      = ggplot2::margin(2, 8, 4, 8)
    )
  
  # Panel 1 · Lake level
  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = Date)) +
    ggplot2::geom_line(ggplot2::aes(y = value), colour = col_level,
                       linewidth = 0.7) +
    x_scale +
    ggplot2::labs(y = "Lake level (masl)") +
    base_theme
  
  # Panel 2 · Lake volume
  p2 <- ggplot2::ggplot(df, ggplot2::aes(x = Date)) +
    ggplot2::geom_area(ggplot2::aes(y = V), fill = col_volume, alpha = 0.3,
                       colour = col_volume, linewidth = 0.5) +
    x_scale +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::labs(y = expression("Volume (m"^3*")")) +
    base_theme
  
  # Panel 3 · Dodged + stacked flux chart
  # Two bars per time step, side by side:
  #   Left  (Inputs): Inflow stacked under Rain
  #   Right (Losses): Outflow + Spill + Evaporation stacked
  # Each group uses position_stack independently via separate geom_col calls.
  flux_cols <- c(
    Inflow      = col_inflow,
    Rain        = col_rain,
    Evaporation = col_evap,
    Outflow     = col_outflow,
    Spill       = col_spill
  )
  
  # Offset inputs left and losses right by half a bar-gap
  bar_width  <- switch(period, daily = 0.4,  monthly = 10,  annual = 120)
  bar_offset <- switch(period, daily = 0.25, monthly = 6.5, annual = 80)
  
  inputs <- df |>
    dplyr::select(Date, Inflow = inflow, Rain = rain) |>
    tidyr::pivot_longer(-Date, names_to = "Component", values_to = "flux_m3") |>
    dplyr::mutate(
      Component  = factor(Component, levels = c("Rain", "Inflow")),
      Date_dodge = Date - bar_offset   # shift left
    )
  
  losses <- df |>
    dplyr::select(Date, Evaporation = evap_m3,
                  Outflow = HYD_outflow, Spill = spill_outflow) |>
    tidyr::pivot_longer(-Date, names_to = "Component", values_to = "flux_m3") |>
    dplyr::mutate(
      Component  = factor(Component, levels = c("Spill", "Outflow", "Evaporation")),
      Date_dodge = Date + bar_offset   # shift right
    )
  
  y_lab_flux <- switch(
    period,
    daily   = expression("Flux (m"^3*" day"^-1*")"),
    monthly = expression("Flux (m"^3*" month"^-1*")"),
    annual  = expression("Flux (m"^3*" yr"^-1*")")
  )
  
  p3 <- ggplot2::ggplot() +
    # Inputs: stacked bars shifted left
    ggplot2::geom_col(
      data     = inputs,
      mapping  = ggplot2::aes(x = Date_dodge, y = flux_m3, fill = Component),
      position = ggplot2::position_stack(),
      width    = bar_width, alpha = 0.85
    ) +
    # Losses: stacked bars shifted right
    ggplot2::geom_col(
      data     = losses,
      mapping  = ggplot2::aes(x = Date_dodge, y = flux_m3, fill = Component),
      position = ggplot2::position_stack(),
      width    = bar_width, alpha = 0.85
    ) +
    x_scale +
    ggplot2::scale_fill_manual(
      values = flux_cols,
      breaks = c("Inflow", "Rain", "Evaporation", "Outflow", "Spill")
    ) +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::labs(y = y_lab_flux, fill = NULL) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2)) +
    base_theme +
    ggplot2::theme(
      legend.position = "bottom",
      axis.title.x    = ggplot2::element_blank(),
      axis.text.x     = ggplot2::element_blank(),
      axis.ticks.x    = ggplot2::element_blank()
    )
  
  # Panel 4 · delta V and net flux
  p4 <- ggplot2::ggplot(df, ggplot2::aes(x = Date)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey60", linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(y = deltaV,  colour = "deltaV (modelled)"),
                       linewidth = 0.7) +
    ggplot2::geom_line(ggplot2::aes(y = net_flux, colour = "Net flux (estimated)"),
                       linewidth = 0.6, linetype = "dashed") +
    x_scale +
    ggplot2::scale_colour_manual(
      values = c("deltaV (modelled)"    = col_deltaV,
                 "Net flux (estimated)" = col_net)
    ) +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::labs(
      y      = expression(paste(Delta, "V / Net flux (m"^3*")")),
      x      = x_lab,
      colour = NULL
    ) +
    bottom_theme +
    ggplot2::theme(legend.position = "bottom")
  
  # Subtitle tag for aggregation
  agg_label <- switch(period,
                      daily   = "",
                      monthly = "  |  Monthly totals",
                      annual  = "  |  Annual totals"
  )
  
  # Assemble with patchwork
  p1 / p2 / p3 / p4 +
    patchwork::plot_layout(heights = c(1, 1, 1.4, 1.2)) +
    patchwork::plot_annotation(
      title    = "Lake Water Balance Diagnostics",
      subtitle = paste0("Model: ", model_name, agg_label),
      theme    = ggplot2::theme(
        plot.title    = ggplot2::element_text(face = "bold", size = 14),
        plot.subtitle = ggplot2::element_text(colour = "grey40")
      )
    )
}

# ── Main function ────────────────────────────────────────────────────────────
#' Plot water balance diagnostics for a single model.
#'
#' @param wb        data.frame with columns as in str(wb). Must contain exactly
#'                  one unique value in the model column.
#' @param time_axis one of `daily`, `monthly`, `annual`, or `auto` (default). 
#' `auto` selects resolution by series length: <= 2 yr -> daily, <= 6 yr -> 
#' monthly, > 6 yr -> annual.
#'
#' @return A patchwork object.
#' @noRd
plot_water_balance <- function(
    wb,
    time_axis = c("auto", "daily", "monthly", "annual")
) {
  
  time_axis <- match.arg(time_axis)
  
  # Enforce single model
  models <- unique(wb$model)
  if (length(models) > 1) {
    stop(
      "wb contains ", length(models), " models: ",
      paste(models, collapse = ", "), ".\n",
      "Please subset to a single model before calling plot_water_balance().\n",
      "  e.g. plot_water_balance(dplyr::filter(wb, model == \"", models[1], "\"))",
      call. = FALSE
    )
  }
  
  model_name <- models[1]
  agg        <- .aggregate_wb(wb, time_axis)
  plt        <- .build_model_plot(agg$data, model_name, agg$x_lab,
                                  agg$period, agg$aggregated)
  plt
}
