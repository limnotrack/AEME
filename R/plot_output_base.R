#' Base plotting function for AEME output
#'
#' @inheritParams plot_output
#' @param plot_width numeric; width in pixels of each panel, used to size the
#' plotting device. Default is 400.
#' @param plot_height numeric; height in pixels of each panel, used to size
#' the plotting device. Default is 200.
#' @param bar_width numeric; width of the colour bar as a fraction of
#' `plot_width`. Default is 0.08.
#' @export
#' @return A list of matrices with the plotted output, invisibly.
#' @importFrom graphics axis box image layout legend lines mtext par
#' @importFrom grDevices dev.new dev.size
#' @examples
#' \dontrun{
#' tmpdir <- tempdir()
#' aeme_dir <- system.file("extdata/lake/", package = "AEME")
#' # Copy files from package into tempdir
#' file.copy(aeme_dir, tmpdir, recursive = TRUE)
#' path <- file.path(tmpdir, "lake")
#' aeme <- yaml_to_aeme(path = path, "aeme.yaml")
#' path <- tempdir()
#' aeme <- build_aeme(aeme = aeme, model = model, path = path, 
#'                    ext_elev = 5) |>
#'   run_aeme(aeme)
#' 
#' plot_output_base(aeme)
#' 
#' # Can also use plot_output() with `backend` set to "base"
#' plot_output(aeme, backend = "base")
#' }

plot_output_base <- function(aeme, var_sim = "HYD_temp", model, ens_n = 1,
                             var_lims = NULL, ylim = NULL,
                             plot_width = 400, plot_height = 200,
                             bar_width = 0.08) {
  
  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")
  
  # --- Input checks --------------------------------------------------------
  aeme <- check_aeme(aeme)
  if (missing(model)) {
    model <- list_models(aeme)
  } else {
    model <- check_model(model = model)
  }
  var_sim <- check_aeme_vars(var_sim, aeme = aeme)

  # --- Enforce one-or-many constraint --------------------------------------
  if (length(var_sim) > 1 && length(model) > 1)
    stop("Supply either one var_sim with multiple models, ",
         "or multiple var_sim with one model, not both.")

  outp    <- output(aeme)
  tme     <- time(aeme)
  ens_lab <- format_ens_label(ens_n = ens_n)

  # --- Check var_sim exists in each model ----------------------------------
  chk <- sapply(model, \(m)
                all(var_sim %in% names(outp[[ens_lab]][[m]]))
  )
  if (all(!chk)) stop("Variable(s) '", paste(var_sim, collapse = ", "),
                      "' not found in any model output")
  if (any(!chk)) {
    warning("Variable(s) missing from: ", paste(model[!chk], collapse = ", "))
    model <- model[chk]
  }

  # --- Guard against non-standard-dimension variables ----------------------
  # (e.g. nzones, particle, sed_layers) -- not supported by this plotting
  # function yet; point users to get_var() instead of failing deep inside
  # the contour/line plotting logic below
  grouped_hits <- unlist(lapply(model, \(m) {
    vapply(var_sim, \(v) {
      inherits(outp[[ens_lab]][[m]][[v]], "aeme_grouped_var")
    }, logical(1))
  }))
  if (any(grouped_hits)) {
    cli::cli_abort(c(
      "x" = "{.arg var_sim} includes variable(s) with non-standard dimensions that {.fn plot_output_base} does not support yet.",
      "i" = "Use {.fn get_var} to retrieve their values directly."
    ), class = "aeme_error_grouped_var_plot")
  }
  
  # --- Date range ----------------------------------------------------------
  dates    <- as.Date(outp[[ens_lab]][[model[1]]][["Date"]])
  xlim     <- c(as.Date(tme$start), as.Date(tme$stop))
  date_idx <- which(dates >= xlim[1] & dates <= xlim[2])
  dates    <- dates[date_idx]
  
  # --- Get raw matrix output -----------------------------------------------
  out    <- get_output(aeme, model, var_sim, ens_lab, date_idx)
  depths <- get_depths(aeme, model, ens_lab, date_idx)
  level  <- get_level(aeme, model, ens_lab, date_idx)
  
  # has_depth: check dimensions of output to determine if depth profiles are present
  has_depth <- any(sapply(out, \(m) any(sapply(m, is.matrix))))
  
  # --- Layout: rows = models, cols = vars (one dimension is always 1) ------
  n_panels <- length(model) * length(var_sim)
  dpi    <- 96
  
  if (has_depth) {
    n_vars <- length(var_sim)
    n_mods <- length(model)
    n_panels <- n_vars * n_mods
    
    # Arrange into squarest possible grid
    n_cols <- ceiling(sqrt(n_panels))
    n_rows <- ceiling(n_panels / n_cols)
    
    bar_px <- plot_width * bar_width
    
    dev_width  <- (n_cols * (plot_width + bar_px)) / dpi
    dev_height <- (n_rows * plot_height) / dpi
    
    if (.Device == "null device" ||
        dev.size("in")[1] < dev_width ||
        dev.size("in")[2] < dev_height) {
      dev.new(width = dev_width, height = dev_height)
    }
    
    col_seq <- rep(c(plot_width, bar_px), n_cols)
    
    # Build layout matrix: each plot/bar pair gets consecutive numbers
    layout_mat <- matrix(0, nrow = n_rows, ncol = n_cols * 2)
    panel_num <- 1
    for (row in seq_len(n_rows)) {
      for (col in seq_len(n_cols)) {
        if (panel_num <= n_panels * 2) {
          layout_mat[row, col * 2 - 1] <- panel_num       # plot
          layout_mat[row, col * 2]     <- panel_num + 1   # its colourbar
          panel_num <- panel_num + 2
        }
      }
    }
    # Map each panel number to its grid position
    
    layout(layout_mat, widths = col_seq)
    
    # Iterate in same row-major order
    combos <- expand.grid(v = var_sim, m = model, stringsAsFactors = FALSE)
    for (i in seq_len(nrow(combos))) {
      m <- combos$m[i]
      v <- combos$v[i]
      
      # Calculate which row/col this panel belongs to
      panel_row  <- ceiling(i / n_cols)
      panel_col  <- ((i - 1) %% n_cols) * 2 + 1  # odd cols are plots
      
      par(mfg = c(panel_row, panel_col))
      par(mar = c(5, 4, 3, 1))
      
      pal     <- get_hm_palette(v, n = 64)
      v_lims  <- if (!is.null(var_lims)) var_lims else
        range(out[[m]][[v]], na.rm = TRUE)
      breaks     <- seq(v_lims[1], v_lims[2], length.out = length(pal) + 1)
      var_name    <- rename_modelvars(input = v, type_output = "name_text")
      panel_title <- if (length(model) > 1) toggle_models(m, to = "display") else var_name
      
      .plot_contour(dates      = dates,
                    mat        = out[[m]][[v]],
                    depth_mat  = depths[[m]],
                    level_vec  = level[[m]],
                    mod_name   = panel_title,
                    pal        = pal,
                    breaks     = breaks,
                    v_lims     = v_lims,
                    xlim       = xlim)
      
      # Colour bar goes in the immediately adjacent even column
      par(mfg = c(panel_row, panel_col + 1))
      
      .draw_colourbar(pal         = pal,
                      v_lims      = v_lims,
                      label       = var_name,
                      bar_px      = bar_px,
                      plot_height = plot_height)
    }
  } else {
    n_panels <- length(var_sim)
    n_cols   <- ceiling(sqrt(n_panels))
    n_rows   <- ceiling(n_panels / n_cols)
    
    dev_width  <- (n_cols * plot_width) / dpi
    dev_height <- (n_rows * plot_height) / dpi
    
    if (.Device == "null device" ||
        dev.size("in")[1] < dev_width ||
        dev.size("in")[2] < dev_height) {
      dev.new(width = dev_width, height = dev_height)
    }
    
    if (is.null(ylim)) {
      vals  <- unlist(lapply(model, \(m) out[[m]][var_sim]))
      ylim  <- range(vals[is.finite(vals)], na.rm = TRUE)
    }
    
    par(mfrow = c(n_rows, n_cols), mar = c(5, 4, 3, 1))
    .plot_line(out = out, dates = dates, model = model,
               var_sim = var_sim, ylim = ylim, xlim = xlim)
  }
  
  invisible(out)
}


# --- Get raw matrix output -----------------------------------------------
#' @noRd
get_output <- function(aeme, model, var_sim, ens_lab, date_idx) {
  outp <- output(aeme)
  names(model) <- model
  lapply(model, \(m) {
    names(var_sim) <- var_sim
    lapply(var_sim, \(v) {
      x <- outp[[ens_lab]][[m]][[v]]
      if (is.matrix(x)) {
        x[, date_idx, drop = FALSE]
      } else {
        x[date_idx]
      }
    })
  })
}

#' @noRd
get_depths <- function(aeme, model, ens_lab, date_idx) {
  outp <- output(aeme)
  names(model) <- model
  lapply(model, \(m)
         outp[[ens_lab]][[m]][["LKE_depths"]][, date_idx, drop = FALSE]
  )
}

#' @noRd
get_level <- function(aeme, model, ens_lab, date_idx) {
  outp <- output(aeme)
  names(model) <- model
  lapply(model, \(m)
         outp[[ens_lab]][[m]][["LKE_lvlwtr"]][date_idx, drop = FALSE]
  )
}


# --- Contour plot --------------------------------------------------------
#' @noRd
.plot_contour <- function(dates, mat, depth_mat, level_vec, mod_name,
                          pal, breaks, v_lims, ylim = NULL, xlim) {
  depth_vec <- apply(depth_mat, 1, median, na.rm = TRUE)
  level_vec <- level_vec - level_vec[1]
  y_range   <- if (!is.null(ylim)) ylim else
    range(c(depth_vec, level_vec), na.rm = TRUE)
  y_range   <- y_range[order(y_range, decreasing = TRUE)]
  
  image(x = as.numeric(dates), y = depth_vec, z = t(mat),
        col = pal, breaks = breaks,
        xlim = as.numeric(xlim), ylim = y_range,
        xlab = "", ylab = "Depth (m)", main = mod_name, axes = FALSE)
  
  lines(as.numeric(dates), level_vec, col = "black", lwd = 1.5)
  
  at_x <- pretty(dates)
  axis(1, at = as.numeric(at_x),
       labels = format(as.Date(at_x, origin = "1970-01-01"), "%b %Y"),
       las = 2, cex.axis = 0.8)
  axis(2); box()
}


# --- Colour bar ----------------------------------------------------------
#' @noRd
.draw_colourbar <- function(pal, v_lims, label, bar_px, plot_height) {
  # Scale margins proportionally to pixel dimensions
  # baseline margins assume 200px height and 60px bar width
  h_scale <- plot_height / 200
  w_scale <- bar_px / 60
  
  max_chars  <- max(nchar(format(round(v_lims, 2))))
  right_mar  <- max(2.5, max_chars * 0.4)
  
  op <- par(mar = c(4 * h_scale, 0.2 * w_scale, 2 * h_scale, right_mar))
  on.exit(par(op))
  
  tryCatch({
    image(x = 1,
          y = seq(v_lims[1], v_lims[2], length.out = 64),
          z = matrix(seq(v_lims[1], v_lims[2], length.out = 64), nrow = 1),
          col = pal, axes = FALSE, xlab = "", ylab = "")
    axis(4, las = 1, cex.axis = 1)
    mtext(label, side = 4, line = right_mar - 0.5, cex = 0.7)
  }, error = \(e) {
    cli::cli_alert_warning("Colour bar too narrow -- try increasing {.arg plot_width}")
  })
}


# --- Line plot -----------------------------------------------------------
#' @noRd
.plot_line <- function(out, dates, model, var_sim, ylim, xlim) {
  cols <- setNames(
    RColorBrewer::brewer.pal(max(3, length(model)), "Set1")[seq_along(model)],
    model
  )
  
  for (v in var_sim) {
    var_name <- rename_modelvars(input = v, type_output = "name_text")
    y_range  <- if (!is.null(ylim)) ylim else
      range(unlist(lapply(model, \(m) out[[m]][[v]])), na.rm = TRUE)
    
    plot(NULL, xlim = as.numeric(xlim), ylim = y_range,
         xlab = "", ylab = var_name, main = var_name, xaxt = "n")
    
    at_x <- pretty(dates)
    axis(1, at = as.numeric(at_x),
         labels = format(as.Date(at_x, origin = "1970-01-01"), "%b %Y"),
         las = 2, cex.axis = 0.8)
    
    for (m in model)
      lines(as.numeric(dates), out[[m]][[v]], col = cols[m], lwd = 1.5)
    
    if (length(model) > 1)
      legend("topright", legend = model, col = cols, lwd = 1.5,
             bty = "n", cex = 0.8)
  }
}

#' @noRd
.open_device <- function(n_vars, n_mods,
                         panel_width  = 4,
                         bar_width    = 0.6,
                         panel_height = 3) {
  dev_width  <- n_vars * (panel_width + bar_width)
  dev_height <- n_mods * panel_height
  
  # Only open a new device if none is available or current is too small
  if (.Device == "null device" ||
      dev.size("in")[1] < dev_width ||
      dev.size("in")[2] < dev_height) {
    dev.new(width = dev_width, height = dev_height)
  }
}
