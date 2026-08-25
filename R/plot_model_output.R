#' Plot a variable from model output -- an `Aeme` object or a raw output list
#'
#' A lightweight ggplot2 plotting helper that works either directly on the
#' list returned by [read_glm_output()], [read_gotm_output()],
#' [read_simstrat_output()], or [read_model_outputs()] -- for when you don't
#' have (or don't want to build) a full `Aeme` object -- or on an `Aeme`
#' object itself, in which case [get_var()]/[plot_var()] do the work for
#' you. Dispatches on the shape of the requested variable: a depth x time
#' tile plot for `(z, time)` variables (same rendering as [plot_output()]'s
#' default backend), a simple time series for `(time)`-only variables (e.g.
#' evaporation), and a line plot (one line per combination of its non-time
#' dimensions) for a [new_grouped_var()] variable, i.e. one with dimensions
#' other than `(time)`/`(z, time)` -- e.g. one line per sediment zone for a
#' GLM-AED `(nzones, time)` AED flux variable.
#'
#' For observation overlay, faceting across multiple models/variables at
#' once, or the base-graphics backend, use [plot_output()] instead -- this
#' function is intentionally minimal, and does not (yet) support
#' `aeme_grouped_var` variables when called on an `Aeme` object through
#' [plot_output()]/[plot_output_base()].
#'
#' @param x either an `Aeme` object, or a list as returned by
#'   [read_glm_output()]/[read_gotm_output()]/[read_simstrat_output()]/
#'   [read_model_outputs()].
#' @param var_sim character; name of the variable to plot, as it appears in
#'   `names(x)` (or in the model output list, if `x` is an `Aeme` object) --
#'   a `var_aeme` name if [key_naming] has a translation for it, otherwise
#'   its raw model/netCDF name.
#' @param model character; model to plot, when `x` is an `Aeme` object with
#'   more than one model. Ignored (and unnecessary) when `x` is already a
#'   raw output list. Defaults to the first model in [list_models()] if not
#'   supplied.
#' @param ens_n integer; ensemble member to plot, when `x` is an `Aeme`
#'   object. Default `1`.
#' @param remove_spin_up logical; when `x` is an `Aeme` object, drop the
#'   spin-up period before plotting (see [get_date_index()]). Default `TRUE`.
#'   Ignored when `x` is already a raw output list -- trim it yourself first
#'   if needed.
#' @param var_lims numeric vector of length 2; colour scale limits for a
#'   depth x time plot. Default `NULL` (ranged to the data).
#' @param ylim numeric vector of length 2; y-axis limits for a line plot.
#'   Default `NULL` (ranged to the data).
#'
#' @return A ggplot2 object (the long-format data frame instead, with a
#'   warning, for a grouped variable that has no `Date` dimension at all).
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_line coord_cartesian labs theme_bw
#'
#' @examples
#' \dontrun{
#' # On a raw output list
#' out <- read_glm_output(file = outfile)
#' plot_model_output(out, "HYD_temp")        # depth x time tile plot
#' plot_model_output(out, "LKE_lvlwtr")      # simple time series
#' plot_model_output(out, "SDF_Fsed_oxy_Z")  # one line per sediment zone
#'
#' # On an Aeme object directly
#' plot_model_output(aeme, "HYD_temp", model = "glm_aed")
#' }
plot_model_output <- function(x, var_sim, model = NULL, ens_n = 1,
                              remove_spin_up = TRUE, var_lims = NULL,
                              ylim = NULL) {

  if (inherits(x, "Aeme")) {
    aeme <- check_aeme(x)
    avail_models <- list_models(aeme)
    if (is.null(model)) {
      model <- unname(avail_models[1])
      if (length(avail_models) > 1) {
        cli_inform_safe(c("i" = paste0(
          "Multiple models found in 'aeme'; using '", model,
          "'. Pass 'model' to select another."
        )))
      }
    } else {
      model <- check_model(model = model)
    }

    ens_lab <- format_ens_label(ens_n = ens_n)
    out_chk <- output(aeme)[[ens_lab]][[model]]
    if (is.null(out_chk)) {
      cli::cli_abort("No output found for model {.val {model}} (ensemble {.val {ens_lab}}) -- has {.fn run_aeme} been called?")
    }
    if (!var_sim %in% names(out_chk)) {
      other_vars <- setdiff(names(out_chk), c("Date", "LKE_depths", "ok", "reason"))
      cli::cli_abort(c(
        "x" = "{.val {var_sim}} not found in output for model {.val {model}}.",
        "i" = "Available variables: {.val {other_vars}}"
      ))
    }

    # get_var() already builds the exact long-format data frame plot_var()
    # expects (Date/depth/value/var_sim/Model), and already handles
    # remove_spin_up and the aeme_grouped_var case (returned without a
    # `depth` column, unlike the matrix/vector case) -- reuse it rather
    # than re-deriving any of that here.
    df <- get_var(aeme = aeme, model = model, var_sim = var_sim,
                  ens_n = ens_n, return_df = TRUE,
                  remove_spin_up = remove_spin_up)

    if (!"depth" %in% names(df)) {
      return(.plot_grouped_ggplot(df, var_sim = var_sim, ylim = ylim))
    }

    return(.plot_var_single(df, var_sim = var_sim, var_lims = var_lims,
                            ylim = ylim))
  }

  .plot_model_output_list(x, var_sim = var_sim, var_lims = var_lims, ylim = ylim)
}

#' Append a "raw output" subtitle to a ggplot2 object (or each plot in a
#' list of them), so a plot built from `raw_output = TRUE` data is visibly
#' distinguishable from AEME-standardised output
#' @param p a ggplot2 object, or list of them.
#' @noRd
.add_raw_subtitle <- function(p) {
  subtitle_layer <- ggplot2::labs(subtitle = "raw output (native units/names/depths)")
  if (inherits(p, "gg")) return(p + subtitle_layer)
  if (is.list(p) && length(p) > 0 && all(vapply(p, inherits, logical(1), "gg"))) {
    return(lapply(p, `+`, subtitle_layer))
  }
  # Not a ggplot object (e.g. the data.frame .plot_grouped_ggplot() returns
  # for a grouped variable with no Date dimension) -- pass through unchanged
  p
}

#' Call [plot_var()] on an already-built long-format data frame and unwrap
#' its per-model list return down to a single ggplot2 object
#'
#' [plot_var()] returns a list of one plot per model when `facet = FALSE` --
#' [plot_model_output()] always deals with a single model, so that list is
#' always length 1; unwrap it for a friendlier return value.
#'
#' @param df data.frame; as returned by [get_var()] or built by
#'   `.plot_model_output_list()`.
#' @inheritParams plot_model_output
#' @noRd
.plot_var_single <- function(df, var_sim, var_lims = NULL, ylim = NULL) {
  p <- plot_var(df = df, var_sim = var_sim, ylim = ylim, xlim = range(df$Date),
               var_lims = var_lims, obs = NULL, add_obs = FALSE, facet = FALSE)
  if (is.list(p) && !inherits(p, "gg") && length(p) == 1) p <- p[[1]]
  p
}

#' Plot a grouped (non depth x time) variable's long-format data frame as
#' one line per combination of its non-time dimensions
#'
#' Companion to `.plot_var_single()` for the shape [plot_var()] doesn't
#' handle -- an `aeme_grouped_var`'s dimensions other than `(time)`/`(z,
#' time)` (e.g. `nzones`) don't fit `plot_var()`'s depth/no-depth dispatch.
#'
#' @param df data.frame; either from [as.data.frame()] on an
#'   `aeme_grouped_var` (raw-list path, plus `var_sim`), or [get_var()]'s
#'   own grouped-variable branch (`Aeme` path).
#' @inheritParams plot_model_output
#' @noRd
.plot_grouped_ggplot <- function(df, var_sim, ylim = NULL) {
  if (!"Date" %in% names(df)) {
    cli::cli_warn("{.val {var_sim}} has no time dimension; nothing to plot as a series.")
    return(df)
  }

  group_dims <- setdiff(names(df), c("Date", "value", "var_sim", "Model"))
  ylim_layer <- if (!is.null(ylim)) ggplot2::coord_cartesian(ylim = ylim) else NULL

  if (length(group_dims) == 0) {
    p <- ggplot2::ggplot(df, ggplot2::aes(Date, value)) +
      ggplot2::geom_line() +
      ylim_layer +
      ggplot2::labs(y = var_sim, title = var_sim) +
      ggplot2::theme_bw()
    return(p)
  }

  df$group <- do.call(paste, c(df[group_dims], sep = " / "))
  ggplot2::ggplot(df, ggplot2::aes(Date, value, colour = group)) +
    ggplot2::geom_line() +
    ylim_layer +
    ggplot2::labs(y = var_sim, title = var_sim,
                 colour = paste(group_dims, collapse = ", ")) +
    ggplot2::theme_bw()
}

#' Build a [plot_var()]-shaped long-format data frame from a raw model
#' output list, and dispatch to the matrix/vector/grouped-variable plot path
#'
#' @inheritParams plot_model_output
#' @param out list; a raw model output list (see [plot_model_output()]).
#' @noRd
.plot_model_output_list <- function(out, var_sim, var_lims = NULL,
                                    ylim = NULL) {
  if (!is_aeme_output(out)) {
    cli::cli_abort("{.arg x} must be an {.cls Aeme} object, or the classed list returned by {.fn read_glm_output}/{.fn read_gotm_output}/{.fn read_simstrat_output}/{.fn read_dy_output}/{.fn read_model_outputs} ({.cls aeme_output}/{.cls aeme_output_raw}).")
  }
  if (!var_sim %in% names(out)) {
    other_vars <- setdiff(names(out), c("Date", "LKE_depths", "ok", "reason"))
    cli::cli_abort(c(
      "x" = "{.val {var_sim}} not found in output.",
      "i" = "Available variables: {.val {other_vars}}"
    ))
  }
  raw      <- is_aeme_output_raw(out)
  dates    <- as.Date(out[["Date"]])
  variable <- out[[var_sim]]

  if (inherits(variable, "aeme_grouped_var")) {
    gdf <- as.data.frame(variable)
    gdf$var_sim <- var_sim
    gdf$Model <- "Output"
    p <- .plot_grouped_ggplot(gdf, var_sim = var_sim, ylim = ylim)
    return(if (raw) .add_raw_subtitle(p) else p)
  }

  if (is.matrix(variable)) {
    depth_mat <- out[["LKE_depths"]]
    if (is.null(depth_mat)) {
      cli::cli_abort("output is missing {.val LKE_depths}, needed to plot a depth x time variable.")
    }

    if (identical(dim(variable), dim(depth_mat))) {
      # Defensive catch: GLM can pad an inactive/unused layer with a giant
      # sentinel fill value (e.g. 9.96921e+36) in one of these two
      # matrices while it's already NA in the other -- ncdf4's automatic
      # missing-value substitution doesn't apply uniformly to every
      # variable. Blank out that layer in *both* wherever either looks
      # invalid, so a bogus depth (or a real depth paired with bogus
      # data) never reaches the plot.
      invalid <- is.na(variable) | is.na(depth_mat) |
        abs(variable) > 1e6 | abs(depth_mat) > 1e6
      variable[invalid]  <- NA
      depth_mat[invalid] <- NA
    }

    each <- nrow(variable)
    df <- data.frame(
      Date = rep(dates, each = each),
      depth = as.vector(depth_mat),
      value = as.vector(variable),
      var_sim = var_sim,
      Model = "Output",
      stringsAsFactors = FALSE
    )
  } else {
    df <- data.frame(Date = dates, depth = NA_real_,
                     value = as.vector(variable), var_sim = var_sim,
                     Model = "Output", stringsAsFactors = FALSE)
  }

  p <- .plot_var_single(df, var_sim = var_sim, var_lims = var_lims, ylim = ylim)
  if (raw) .add_raw_subtitle(p) else p
}
