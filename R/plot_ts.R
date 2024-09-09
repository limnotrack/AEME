#' Plot multi-variable timeseries
#'
#' @inheritParams build_aeme
#' @param depth_range numeric; range of depths to average. Default is NULL,
#' which averages over all depths.
#' @param var_sim character; variable in the AEME format (e.g. "HYD_temp"). Can
#' be a vector.
#'
#' @return A ggplot object
#' @export
#'

plot_ts <- function(aeme, model, var_sim, add_obs = TRUE, depth_range = NULL,
                        ens_n = 1) {

  # Get model controls
  model_controls <- get_model_controls(aeme)
  if (is.null(model_controls)) {
    stop("No model controls found")
  }

  sim_vars <- model_controls |>
    dplyr::filter(simulate) |>
    dplyr::pull(var_aeme)

  var_chk <- var_sim %in% sim_vars

  if (!all(var_chk)) {
    stop(strwrap("No variables found in model controls.\nMake sure
                 to set simulate = TRUE for selected variables when
                 executing `run_aeme`.", width = 80))
  }

  # Check model output is present
  chk <- check_var_in_output(aeme = aeme, model = model, var_sim = var_sim,
                             ens_n = ens_n)

  # Get variables
  out_df <- lapply(var_sim, \(v) {
    df <- get_var(aeme = aeme, model = model, var_sim = v)
    if (!is.null(depth_range)) {
      depth_range <- abs(depth_range)
      df <- df |>
        dplyr::group_by(Date, Model) |>
        dplyr::mutate(depth = max(lyr_top) - lyr_top) |>
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

  # Add key naming
  utils::data("key_naming", package = "AEME", envir = environment())
  out_df <- out_df |>
    dplyr::left_join(key_naming[, c("name", "name_parse", "name_text")],
                     by = c("var_sim" = "name"))

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
    obs <- get_obs(aeme = aeme, var_sim = var_sim, depth_range = depth_range)
    if (nrow(obs) > 0) {
      obs <- obs |>
        dplyr::group_by(Date, var_aeme) |>
        dplyr::summarise(value = mean(value), .groups = "drop") |>
        dplyr::left_join(key_naming[, c("name", "name_parse", "name_text")],
                         by = c("var_aeme" = "name"))
      p1 <- p1 +
        ggplot2::geom_point(data = obs, ggplot2::aes(x = Date, y = value,
                                                     fill = "Obs",
                                                     color = name_text),
                            shape = 1, size = 2) +
        ggplot2::labs(fill = "")
    }
  }
  return(p1)
}
