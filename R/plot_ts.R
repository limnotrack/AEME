#' Plot multi-variable timeseries
#'
#' @inheritParams plot_output
#' @inheritParams get_var
#' @param depth_range numeric; range of depths to average. Default is NULL,
#' which averages over all depths.
#' @param var_sim character; variable in the AEME format (e.g. "HYD_temp"). Can
#' be a vector.
#' @param add_obs logical; add observations to the plot. Default is TRUE.
#' @param obs_size numeric; size of the observation points. Default is 2.5.
#' A single variable is shown with black points (white outline); multiple
#' variables are filled to match the line colour (white outline).
#'
#' @importFrom ggplot2 ggplot geom_line guides scale_linewidth_manual
#' @importFrom ggplot2 scale_alpha_manual facet_wrap labs geom_point aes
#' @importFrom dplyr filter group_by mutate summarise left_join
#' @importFrom dplyr bind_rows
#'
#' @return A ggplot object
#' @export
#'

plot_ts <- function(aeme, model, var_sim, remove_spin_up = TRUE,
                    add_obs = TRUE, depth_range = NULL, ens_n = 1,
                    obs_size = 2.5) {

  aeme <- check_aeme(aeme)
  if (missing(model)) {
    model <- list_models(aeme)
  } else {
    model <- check_model(model = model)
  }
  var_sim <- check_aeme_vars(var_sim, aeme = aeme)
  # Get model controls
  model_controls <- get_model_controls(aeme)
  if (is.null(model_controls)) {
    cli::cli_abort("No model controls found. Make sure to set `model_controls` 
                   when executing `build_aeme`.")
  }

  sim_vars <- model_controls |>
    dplyr::filter(simulate) |>
    dplyr::pull(var_aeme)

  var_chk <- var_sim %in% sim_vars

  if (!all(var_chk)) {
    cli::cli_abort(c("No variables found in model controls.",
                     "Make sure to set {.code simulate = TRUE} in the 
                     {.code model_controls} for selected variables when 
                     executing {.code run_aeme}. Use the {.code set_vars_sim()} 
                     to set variables to simulate."))
  }

  # Check model output is present
  chk <- check_var_in_output(aeme = aeme, model = model, var_sim = var_sim,
                             ens_n = ens_n)

  # Get variables
  out_df <- lapply(var_sim, \(v) {
    df <- get_var(aeme = aeme, model = model, var_sim = v, 
                  remove_spin_up = remove_spin_up)
    if (!is.null(depth_range)) {
      depth_range <- abs(depth_range)
      df <- df |>
        dplyr::group_by(Date, Model) |>
        dplyr::filter(depth >= min(depth_range) & depth <= max(depth_range))
    }

    df <- df |>
      dplyr::group_by(Date, Model) |>
      dplyr::summarise(mean = mean(value), sd = sd(value), model = Model[1],
                       var_sim = v, .groups = "drop") |>
      as.data.frame()
    if (all(df$mean == -99)) {
      return(NULL)
    } else {
      return(df)
    }
  }) |>
    dplyr::bind_rows()
  
  if (nrow(out_df) == 0) {
    cli::cli_alert_danger("No data found for the selected variables {.var {var_sim}} and models {.var {model}}.")
    return(NULL)
  }

  # Add key naming
  data("key_naming", package = "AEME", envir = environment())
  out_df <- out_df |>
    dplyr::left_join(key_naming[, c("var_aeme", "name_parse", "name_text")],
                     by = c("var_sim" = "var_aeme"))

  var_lwd <- rep(1, length(var_sim))
  names(var_lwd) <- unique(out_df$name_text)
  var_alpha <- rep(1, length(var_sim))
  names(var_alpha) <- unique(out_df$name_text)

  # Plot
  y_lab <- eval(parse(text = out_df$name_parse[1]))
  p1 <- ggplot2::ggplot(out_df, ggplot2::aes(x = Date, y = mean,
                                             color = name_text)) +
    ggplot2::geom_line(ggplot2::aes(linewidth = name_text, alpha = name_text)) +
    ggplot2::guides(linewidth = "none", alpha = "none") +
    ggplot2::scale_linewidth_manual(values = var_lwd) +
    ggplot2::scale_alpha_manual(values = var_alpha) +
    ggplot2::facet_wrap(~model, scales = "free_y", ncol = 1) +
    ggplot2::labs(x = "Date", y = y_lab, colour = "Variable")

  if (add_obs) {
    obs <- get_obs(aeme = aeme, var_sim = var_sim, depth_range = depth_range,
                   time_filter = TRUE)
    if (nrow(obs) > 0) {
      obs <- obs |>
        dplyr::group_by(Date, var_aeme) |>
        dplyr::summarise(value = mean(value), .groups = "drop") |>
        dplyr::left_join(key_naming[, c("var_aeme", "name_parse", "name_text")],
                         by = "var_aeme")
      # Match the model line layer's x-axis class: obs Date is noon POSIXct,
      # the model series is Date (daily) or POSIXct (sub-daily).
      if (inherits(out_df$Date, "Date")) {
        obs$Date <- as.Date(obs$Date, tz = "UTC")
      }
      lvls <- unique(out_df$name_text)
      if (length(lvls) == 1) {
        # Single variable: black points with a white outline
        p1 <- p1 +
          ggplot2::geom_point(data = obs,
                              ggplot2::aes(x = Date, y = value,
                                           shape = "Observed"),
                              fill = "black", colour = "white",
                              size = obs_size, stroke = 0.8,
                              inherit.aes = FALSE) +
          ggplot2::scale_shape_manual(values = c(Observed = 21), name = NULL)
      } else {
        # Multiple variables: fill matches the line colour, white outline
        p1 <- p1 +
          ggplot2::geom_point(data = obs,
                              ggplot2::aes(x = Date, y = value,
                                           fill = name_text),
                              shape = 21, colour = "white",
                              size = obs_size, stroke = 0.8,
                              inherit.aes = FALSE) +
          ggplot2::scale_colour_discrete(limits = lvls) +
          ggplot2::scale_fill_discrete(limits = lvls) +
          ggplot2::labs(fill = "Variable")
      }
    }
  }
  return(p1)
}
