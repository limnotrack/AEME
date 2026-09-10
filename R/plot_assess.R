#' Plot model performance metrics
#'
#' Visualise the model performance statistics returned by
#' \code{\link{assess_aeme}}. Four complementary views are available via
#' \code{type}:
#' \itemize{
#' \item{\code{"dot"}}{ - Cleveland dot plot, one facet per metric with a
#' free x-axis, points coloured by model. The general-purpose default: it
#' keeps metrics on their own scales so native-unit errors (bias, rmse,
#' nmae) and bounded skill scores (nse, kge, d2, r) are not forced onto a
#' common axis.}
#' \item{\code{"heatmap"}}{ - Tile plot of model (x) against variable (y),
#' faceted by metric. Fill is the value rescaled to `0-1` within each
#' metric/variable and oriented so that `1` is the best-performing model;
#' the raw value is printed in each tile.}
#' \item{\code{"taylor"}}{ - Normalised Taylor diagram (Taylor, 2001), one
#' point per model per variable, encoding the correlation and the ratio of
#' modelled to observed standard deviation. The observations plot at
#' `(1, 0)`.}
#' \item{\code{"target"}}{ - Target diagram (Jolliff et al., 2009), plotting
#' normalised bias against normalised unbiased RMSD. Points inside the unit
#' circle have a total RMSD smaller than the observed standard deviation.}
#' }
#'
#' \code{"dot"} and \code{"heatmap"} are built from \code{\link{assess_aeme}}
#' output; \code{"taylor"} and \code{"target"} are computed from the paired
#' observed/modelled values returned by \code{\link{get_var}}.
#'
#' @inheritParams assess_aeme
#' @inheritParams plot_output
#' @param type character; which view to draw. One of \code{"dot"}
#' (default), \code{"heatmap"}, \code{"taylor"} or \code{"target"}.
#' @param metrics character vector; metrics to include in the \code{"dot"}
#' and \code{"heatmap"} views. Defaults to
#' \code{c("bias", "rmse", "nmae", "nse", "kge", "d2", "r")}. Ignored for
#' \code{"taylor"} and \code{"target"}.
#'
#' @return A ggplot2 object.
#'
#' @seealso \code{\link{assess_aeme}} for the underlying statistics.
#'
#' @importFrom dplyr filter mutate group_by ungroup summarise left_join
#' @importFrom dplyr bind_rows pull all_of select n
#' @importFrom tidyr pivot_longer
#' @importFrom rlang arg_match
#' @importFrom stats sd cor
#' @importFrom ggplot2 ggplot aes geom_point geom_tile geom_text geom_path
#' @importFrom ggplot2 geom_hline geom_vline facet_wrap labs
#' @importFrom ggplot2 scale_colour_viridis_d scale_fill_viridis_c coord_equal
#' @importFrom ggplot2 scale_x_continuous theme_bw theme element_text expansion
#' @importFrom utils data
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   metrics <- assess_aeme(aeme = aeme, model = c("glm_aed", "gotm_wet"))
#'   plot_assess(aeme = aeme, model = c("glm_aed", "gotm_wet"))
#'   plot_assess(aeme = aeme, type = "heatmap")
#'   plot_assess(aeme = aeme, type = "taylor", var_sim = "HYD_temp")
#' }

plot_assess <- function(aeme, model, var_sim,
                        type = c("dot", "heatmap", "taylor", "target"),
                        metrics = c("bias", "rmse", "nmae", "nse", "kge",
                                    "d2", "r")) {

  type <- rlang::arg_match(type)

  # Check aeme is Aeme object
  aeme <- check_aeme(aeme)
  # Check model is valid
  if (missing(model)) {
    model <- list_models(aeme = aeme)
  } else {
    model <- check_model(model = model)
  }
  # Resolve variables the same way assess_aeme() does
  if (missing(var_sim)) {
    var_sim <- get_mod_obs_vars(aeme = aeme, model = model) |>
      dplyr::pull(var_aeme)
    if (length(var_sim) == 0) {
      cli::cli_alert_warning(c("No variables found in model output. ",
                               "Make sure to set {.code simulate = TRUE} in the ",
                               "{.code model_controls} for selected variables ",
                               "when executing {.code run_aeme}."))
      return(NULL)
    }
  }
  var_sim <- check_aeme_vars(var_sim, aeme = aeme)

  # Pretty variable names
  data("key_naming", package = "AEME", envir = environment())
  var_name <- key_naming |>
    dplyr::select(var_aeme, name_text)

  # Higher-is-better orientation for each metric, used by the heatmap and to
  # label the dot-plot facets.
  metric_dir <- c(bias = 0, mae = -1, rmse = -1, nmae = -1, nse = 1, kge = 1,
                  d2 = 1, r = 1, rs = 1, B = 1)

  if (type %in% c("dot", "heatmap")) {

    df <- assess_aeme(aeme = aeme, model = model, var_sim = var_sim)
    if (is.null(df) || nrow(df) == 0) {
      cli::cli_alert_warning("No performance metrics to plot.")
      return(NULL)
    }

    metrics <- intersect(metrics, names(df))
    if (length(metrics) == 0) {
      cli::cli_abort("None of {.arg metrics} are columns in the {.fn assess_aeme} output.")
    }

    long <- df |>
      dplyr::mutate(Model = toggle_models(Model, to = "display")) |>
      tidyr::pivot_longer(cols = dplyr::all_of(metrics), names_to = "metric",
                          values_to = "value") |>
      dplyr::mutate(metric = factor(metric, levels = metrics))

    if (type == "dot") {
      p <- ggplot2::ggplot(long, ggplot2::aes(x = value, y = name_text,
                                              colour = Model)) +
        {if ("bias" %in% metrics)
          ggplot2::geom_vline(data = data.frame(metric = factor("bias",
                                                                levels = metrics)),
                              ggplot2::aes(xintercept = 0), linetype = "dashed",
                              colour = "grey60", inherit.aes = FALSE)} +
        ggplot2::geom_point(size = 2.5, alpha = 0.9) +
        ggplot2::facet_wrap(~metric, scales = "free_x") +
        ggplot2::scale_colour_viridis_d(option = "D", end = 0.9) +
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = 0.15)) +
        ggplot2::labs(x = NULL, y = NULL, colour = "Model",
                      title = "Model performance by metric") +
        ggplot2::theme_bw()
    } else {
      # Rescale within metric x variable so 1 = best model, and orient by the
      # metric's higher-is-better direction (bias uses |bias|).
      long <- long |>
        dplyr::group_by(metric, var_sim) |>
        dplyr::mutate(
          score = {
            v <- value
            dir <- metric_dir[as.character(metric[1])]
            if (isTRUE(dir == 0)) v <- -abs(v) else v <- dir * v
            rng <- range(v, na.rm = TRUE)
            if (!is.finite(diff(rng)) || diff(rng) == 0) {
              rep(0.5, length(v))
            } else {
              (v - rng[1]) / diff(rng)
            }
          }
        ) |>
        dplyr::ungroup()

      p <- ggplot2::ggplot(long, ggplot2::aes(x = Model, y = name_text)) +
        ggplot2::geom_tile(ggplot2::aes(fill = score), colour = "white") +
        ggplot2::geom_text(ggplot2::aes(label = signif(value, 3)), size = 3) +
        ggplot2::facet_wrap(~metric) +
        ggplot2::scale_fill_viridis_c(option = "D", limits = c(0, 1),
                                      name = "Relative\nskill") +
        ggplot2::labs(x = NULL, y = NULL,
                      title = "Model performance by metric",
                      subtitle = "Fill rescaled within each panel (1 = best model)") +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30,
                                                           hjust = 1))
    }

    return(p)
  }

  # ---- Taylor / target diagrams -------------------------------------------
  # Paired-value summary statistics, one row per model per variable.
  stats_df <- lapply(var_sim, \(v) {
    d <- get_var(aeme = aeme, model = model, var_sim = v, use_obs = TRUE)
    if (is.null(d) || nrow(d) == 0) return(NULL)
    d <- d |>
      dplyr::filter(!is.na(sim), !is.infinite(sim), !is.na(obs))
    if (nrow(d) == 0) return(NULL)

    d |>
      dplyr::group_by(Model, var_sim) |>
      dplyr::summarise(
        sd_obs = stats::sd(obs),
        sd_sim = stats::sd(sim),
        r      = suppressWarnings(stats::cor(obs, sim)),
        bias   = mean(sim - obs),
        crmsd  = sqrt(mean(((sim - mean(sim)) - (obs - mean(obs)))^2)),
        n      = dplyr::n(),
        .groups = "drop"
      )
  }) |>
    dplyr::bind_rows()

  if (is.null(stats_df) || nrow(stats_df) == 0) {
    cli::cli_alert_warning("No paired observed/modelled values to plot.")
    return(NULL)
  }

  stats_df <- stats_df |>
    dplyr::filter(is.finite(sd_obs), sd_obs > 0) |>
    dplyr::mutate(
      Model    = toggle_models(Model, to = "display"),
      sd_ratio = sd_sim / sd_obs,
      crmsd_n  = crmsd / sd_obs,
      bias_n   = bias / sd_obs
    ) |>
    dplyr::left_join(var_name, by = c("var_sim" = "var_aeme"))

  if (type == "taylor") {
    stats_df <- stats_df |>
      dplyr::mutate(tx = sd_ratio * r,
                    ty = sd_ratio * sqrt(pmax(0, 1 - r^2)))

    # Dashed arcs at sd ratio = 0.5, 1, 1.5
    arc <- do.call(rbind, lapply(c(0.5, 1, 1.5), \(rr) {
      th <- seq(0, pi / 2, length.out = 100)
      data.frame(radius = rr, ax = rr * cos(th), ay = rr * sin(th))
    }))

    p <- ggplot2::ggplot() +
      ggplot2::geom_path(data = arc,
                         ggplot2::aes(x = ax, y = ay, group = radius),
                         linetype = "dashed", colour = "grey70") +
      ggplot2::geom_point(data = data.frame(tx = 1, ty = 0),
                          ggplot2::aes(x = tx, y = ty), shape = 8, size = 3) +
      ggplot2::geom_point(data = stats_df,
                          ggplot2::aes(x = tx, y = ty, colour = Model),
                          size = 3) +
      ggplot2::facet_wrap(~name_text) +
      ggplot2::scale_colour_viridis_d(option = "D", end = 0.9) +
      ggplot2::coord_equal() +
      ggplot2::labs(x = "Standard deviation (normalised)",
                    y = "Standard deviation (normalised)",
                    colour = "Model",
                    title = "Taylor diagram",
                    subtitle = "✳ = observations; angle encodes correlation") +
      ggplot2::theme_bw()

    return(p)
  }

  # type == "target"
  circ <- data.frame(cx = cos(seq(0, 2 * pi, length.out = 200)),
                     cy = sin(seq(0, 2 * pi, length.out = 200)))
  stats_df <- stats_df |>
    dplyr::mutate(crmsd_sign = crmsd_n * sign(sd_sim - sd_obs))

  p <- ggplot2::ggplot() +
    ggplot2::geom_path(data = circ, ggplot2::aes(x = cx, y = cy),
                       colour = "grey70") +
    ggplot2::geom_hline(yintercept = 0, colour = "grey70") +
    ggplot2::geom_vline(xintercept = 0, colour = "grey70") +
    ggplot2::geom_point(data = stats_df,
                        ggplot2::aes(x = crmsd_sign, y = bias_n, colour = Model),
                        size = 3) +
    ggplot2::facet_wrap(~name_text) +
    ggplot2::scale_colour_viridis_d(option = "D", end = 0.9) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = "Unbiased RMSD (normalised, signed by σ difference)",
                  y = "Bias (normalised)",
                  colour = "Model",
                  title = "Target diagram",
                  subtitle = "Inside the circle: total RMSD < observed σ") +
    ggplot2::theme_bw()

  return(p)
}
