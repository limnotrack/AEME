#' Plot a variable from a raw read_glm_output()/read_model_outputs() list
#'
#' A lightweight plotting helper for the list returned directly by
#' [read_glm_output()] or [read_model_outputs()] (with `model = "glm_aed"`), for when
#' you don't have (or don't want to build) a full `Aeme` object -- e.g.
#' `out <- read_glm_output(file = outfile)`. Dispatches on the shape of the
#' requested variable: a depth x time contour plot for `(z, time)`
#' variables (paired with lake level, same convention as [plot_output()]),
#' a simple time series for `(time)`-only variables, and a line plot (one
#' line per combination of its non-time dimensions) for an
#' [new_grouped_var()] variable -- e.g. one line per sediment zone for a
#' `(nzones, time)` AED flux variable.
#'
#' For richer plots (ggplot2 backend, faceting, observation overlay,
#' multiple models/variables at once), build an `Aeme` object (e.g. via
#' [glm_config_to_aeme()]) and use [plot_output()] instead -- this function
#' is intentionally minimal.
#'
#' @param out list; as returned by [read_glm_output()] or
#'   [read_model_outputs()] (with `model = "glm_aed"`).
#' @param var_sim character; name of the variable to plot, as it appears in
#'   `names(out)` (a `var_aeme` name if [key_naming] has a translation for
#'   it, otherwise its raw GLM/netCDF name).
#' @param var_lims numeric vector of length 2; colour scale limits for a
#'   depth x time plot. Default `NULL` (ranged to the data).
#' @param ylim numeric vector of length 2; y-axis limits for a line plot.
#'   Default `NULL` (ranged to the data).
#'
#' @return Invisibly, the plotted data (the raw variable for a vector/matrix,
#'   or the long-format data frame for a grouped variable).
#' @export
#'
#' @importFrom graphics axis box image legend lines plot
#' @importFrom RColorBrewer brewer.pal
#'
#' @examples
#' \dontrun{
#' out <- read_glm_output(file = outfile)
#' plot_glm_output(out, "HYD_temp")        # depth x time contour
#' plot_glm_output(out, "LKE_lvlwtr")      # simple time series
#' plot_glm_output(out, "SDF_Fsed_oxy_Z")  # one line per sediment zone
#' }
plot_glm_output <- function(out, var_sim, var_lims = NULL, ylim = NULL) {
  if (!is.list(out) || !"Date" %in% names(out)) {
    cli::cli_abort("{.arg out} must be the list returned by {.fn read_glm_output} or {.fn read_model_outputs}.")
  }
  if (!var_sim %in% names(out)) {
    other_vars <- setdiff(names(out), c("Date", "LKE_depths", "ok", "reason"))
    cli::cli_abort(c(
      "x" = "{.val {var_sim}} not found in {.arg out}.",
      "i" = "Available variables: {.val {other_vars}}"
    ))
  }
  dates    <- as.Date(out[["Date"]])
  variable <- out[[var_sim]]

  if (inherits(variable, "aeme_grouped_var")) {
    return(invisible(.plot_glm_grouped(variable, var_sim, dates, ylim)))
  }

  if (is.matrix(variable)) {
    depth_mat <- out[["LKE_depths"]]
    level_vec <- out[["LKE_lvlwtr"]]
    if (is.null(depth_mat) || is.null(level_vec)) {
      cli::cli_abort("{.arg out} is missing {.val LKE_depths}/{.val LKE_lvlwtr}, needed to plot a depth x time variable.")
    }
    pal    <- get_hm_palette(var_sim, n = 64)
    v_lims <- if (!is.null(var_lims)) var_lims else range(variable, na.rm = TRUE)
    breaks <- seq(v_lims[1], v_lims[2], length.out = length(pal) + 1)
    .plot_contour(dates = dates, mat = variable, depth_mat = depth_mat,
                 level_vec = level_vec, mod_name = var_sim, pal = pal,
                 breaks = breaks, v_lims = v_lims, xlim = range(dates))
    return(invisible(variable))
  }

  # Plain (time) vector
  y_range <- if (!is.null(ylim)) ylim else range(variable, na.rm = TRUE)
  plot(dates, as.vector(variable), type = "l", xlab = "", ylab = var_sim,
      main = var_sim, ylim = y_range)
  invisible(variable)
}

#' @param gv an `aeme_grouped_var` object.
#' @param var_sim character; variable name, used as the plot title/y-axis
#'   label.
#' @param dates Date vector; not currently used (the variable's own `Date`
#'   dimension, if present, is used instead) -- kept for a consistent
#'   internal signature with the vector/matrix branches.
#' @noRd
.plot_glm_grouped <- function(gv, var_sim, dates, ylim) {
  df <- as.data.frame(gv)
  group_dims <- setdiff(names(df), c("Date", "value"))

  if (length(group_dims) == 0 || !"Date" %in% names(df)) {
    # No grouping dimension besides time (rare) -- just a line, or if
    # there's no time dimension at all, nothing sensible to plot as a
    # series
    if ("Date" %in% names(df)) {
      y_range <- if (!is.null(ylim)) ylim else range(df$value, na.rm = TRUE)
      plot(df$Date, df$value, type = "l", xlab = "", ylab = var_sim,
          main = var_sim, ylim = y_range)
    } else {
      cli::cli_warn("{.val {var_sim}} has no time dimension; nothing to plot as a series.")
    }
    return(df)
  }

  df$group <- do.call(paste, c(df[group_dims], sep = " / "))
  groups   <- unique(df$group)
  cols <- RColorBrewer::brewer.pal(max(3, length(groups)), "Set1")[seq_along(groups)]
  y_range <- if (!is.null(ylim)) ylim else range(df$value, na.rm = TRUE)

  plot(NULL, xlim = range(df$Date), ylim = y_range, xlab = "", ylab = var_sim,
      main = paste0(var_sim, " (by ", paste(group_dims, collapse = ", "), ")"),
      xaxt = "n")
  at_x <- pretty(df$Date)
  axis(1, at = as.numeric(at_x),
      labels = format(as.Date(at_x, origin = "1970-01-01"), "%b %Y"),
      las = 2, cex.axis = 0.8)
  for (i in seq_along(groups)) {
    sub <- df[df$group == groups[i], ]
    lines(sub$Date, sub$value, col = cols[i], lwd = 1.5)
  }
  legend("topright", legend = groups, col = cols, lwd = 1.5, bty = "n",
        cex = 0.8)
  df
}
