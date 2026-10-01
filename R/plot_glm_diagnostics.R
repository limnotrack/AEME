#' Plot GLM-AED diagnostic variables to debug unrealistic oxygen values
#'
#' Reads model output using read_model_outputs() and produces a multi-panel
#' diagnostic plot organised into thematic groups: oxygen budget, physical
#' drivers, mixing, biological demand, and sediment fluxes.
#'
#' @param aeme An AEME object. Required if \code{lake_dir} is not provided.
#' @param lake_dir Character. Path to the lake model output directory. If
#'   missing, derived from \code{aeme} and \code{path}.
#' @param surface_depth Numeric. Depth (m) to extract for "surface" values.
#'   Defaults to 0.5.
#' @param bottom_depth Numeric or NULL. Depth (m) to extract for "bottom"
#'   values. If NULL, the deepest available layer is used. Defaults to NULL.
#' @param dates Date vector or NULL. Subset of dates to plot. NULL = all.
#' @param phyto_pars Dataframe of phytoplankton parameters (passed through to
#'   read_model_outputs). Defaults to NULL.
#' @param output_dir Character or NULL. Directory to save PDF output. If NULL,
#'   plots are displayed interactively. Defaults to NULL.
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_hline labs theme_minimal
#' @importFrom ggplot2 theme scale_colour_manual element_text element_blank 
#' @importFrom ggplot2 annotate theme_void ggsave
#' @importFrom patchwork wrap_plots plot_annotation patchworkGrob
#' @importFrom cli cli_abort cli_alert_success
#'
#' @return Invisibly returns a list containing the \code{out} object from
#'   read_model_outputs and the assembled ggplot page objects.
#'
#' @export

plot_glm_diagnostics <- function(aeme, lake_dir,
                                 surface_depth = 0.5,
                                 bottom_depth = NULL,
                                 dates = NULL,
                                 phyto_pars = NULL,
                                 output_dir = NULL) {

  if (missing(aeme) & missing(lake_dir)) {
    cli::cli_abort(
      "Must provide either {.arg lake_dir} or both {.arg aeme} and {.arg path}."
    )
  }  
  if (missing(lake_dir)) {
    path <- get_aeme_path(aeme)
    lake_dir <- get_lake_dir(aeme, path)
  }
  model <- "glm_aed"
  
  # ---- 0. Variable groups ------------------------------------------------
  diag_vars_nc <- c(
    # Oxygen
    "OXY_oxy", "OXY_sat", "OXY_oxy_atmv", "OXY_oxy_dsfv",
    # Atmospheric exchange (areal)
    "OXY_oxy_atm", "OXY_oxy_dsf",
    # Physical
    "temp", "wind", "surface_temp", "CHE",
    "daily_qsw", "daily_qe", "daily_qh", "daily_qlw",
    # Mixing / structure
    "NS", "lake_number", "max_dT_dz", "H", "V",
    # Biological oxygen demand
    "OGM_doc", "OGM_doc_min", "OGM_doc_anaerobic", "OGM_doc_denit",
    "OGM_bod5",
    "PHY_cyano_rsp_c", "PHY_green_rsp_c", "PHY_diatom_rsp_c",
    "PHY_cyano_gpp_c", "PHY_green_gpp_c", "PHY_diatom_gpp_c",
    "PHY_tphy", "PHY_tchla",
    # Sediment
    "SDF_Fsed_oxy", "SDF_Fsed_amm", "SDF_Fsed_nit", "SDF_Fsed_frp"
  )
  
  # ---- 1. Read output -----------------------------------------------------
  nc_file <- get_model_outfile(model = model, path = lake_dir)[[model]]
  if (!file.exists(nc_file)) {
    cli::cli_abort("GLM output file not found at {.file {nc_file}}")
  }
  
  out <- read_model_outputs(lake_dir = lake_dir, model = model,
                            vars_sim = diag_vars_nc, dates = dates,
                            phyto_pars = phyto_pars)
  
  datetime <- out[["Date"]]
  depths   <- out[["LKE_depths"]]  # matrix: depth x time
  nt       <- length(datetime)
  
  # ---- 2. Helpers: extract surface/bottom from 2D matrices ----------------
  
  extract_at_depth <- function(mat, target_depth) {
    if (is.null(mat) || !is.matrix(mat)) return(NULL)
    vapply(seq_len(nt), function(i) {
      d <- depths[, i]
      valid <- !is.na(d)
      d <- d[valid]
      v <- mat[valid, i]
      if (length(d) == 0) return(NA_real_)
      closest <- which.min(abs(d - target_depth))
      v[closest]
    }, numeric(1))
  }
  
  extract_surface <- function(mat) extract_at_depth(mat, surface_depth)
  
  extract_bottom <- function(mat) {
    if (is.null(mat) || !is.matrix(mat)) return(NULL)
    vapply(seq_len(nt), function(i) {
      d <- depths[, i]
      valid <- !is.na(d)
      d <- d[valid]
      v <- mat[valid, i]
      if (length(d) == 0) return(NA_real_)
      if (!is.null(bottom_depth)) {
        closest <- which.min(abs(d - bottom_depth))
      } else {
        closest <- which.max(d)  # deepest layer
      }
      v[closest]
    }, numeric(1))
  }
  
  # Safely pull a variable, returning NULL if missing
  safe_var <- function(name) out[[name]]
  
  # ---- 3. Plotting helpers ------------------------------------------------
  
  # Build a data.frame from named time-series vectors
  make_ts_df <- function(..., date = datetime) {
    args <- list(...)
    dfs <- lapply(names(args), function(nm) {
      v <- args[[nm]]
      if (is.null(v) || all(is.na(v))) return(NULL)
      data.frame(Date = date, value = as.numeric(v), variable = nm,
                 stringsAsFactors = FALSE)
    })
    do.call(rbind, Filter(Negate(is.null), dfs))
  }
  
  # Single-panel ggplot time series
  ts_gg <- function(df, ylab = "", title = "", colours = NULL,
                    hline = NULL, linetypes = NULL) {
    if (is.null(df) || nrow(df) == 0) {
      return(
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5,
                            label = paste(title, "\n(not available)"),
                            colour = "grey50", size = 4) +
          ggplot2::theme_void() +
          ggplot2::labs(title = title)
      )
    }
    
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$Date, y = .data$value,
                                          colour = .data$variable)) +
      ggplot2::theme_minimal(base_size = 10) +
      ggplot2::labs(x = NULL, y = ylab, title = title) +
      ggplot2::theme(
        legend.title = ggplot2::element_blank(),
        legend.position = if (length(unique(df$variable)) > 1) "top" else "none",
        plot.title = ggplot2::element_text(size = 10, face = "bold")
      )
    
    if (!is.null(linetypes)) {
      p <- p + ggplot2::geom_line(ggplot2::aes(linetype = .data$variable))
    } else {
      p <- p + ggplot2::geom_line()
    }
    
    if (!is.null(colours)) {
      p <- p + ggplot2::scale_colour_manual(values = colours)
    }
    if (!is.null(hline)) {
      p <- p + ggplot2::geom_hline(yintercept = hline, linetype = 3,
                                   colour = "red", linewidth = 0.4)
    }
    p
  }
  
  # ---- Colours -----------------------------------------------------------
  col_surf  <- "#1b9e77"
  col_bott  <- "#d95f02"
  col_flux  <- "#7570b3"
  col_grey  <- "#636363"
  col_heat  <- c("Qsw" = "#e41a1c", "Qe" = "#377eb8",
                 "Qh" = "#4daf4a", "Qlw" = "#984ea3")
  col_phyto <- c("cyano" = "#e41a1c", "green" = "#4daf4a",
                 "diatom" = "#377eb8")
  
  # ===== Page 1: Oxygen state & key drivers ===============================
  p1a <- ts_gg(
    make_ts_df(
      "OXY_oxy (surf)" = extract_surface(safe_var("OXY_oxy")),
      "OXY_sat (surf)"  = extract_surface(safe_var("OXY_sat"))
    ),
    ylab = "mmol O2/m\u00b3", title = "Surface dissolved oxygen",
    colours = c("OXY_oxy (surf)" = col_surf, "OXY_sat (surf)" = col_grey),
    hline = 0, linetypes = TRUE
  )
  
  p1b <- ts_gg(
    make_ts_df("OXY_oxy (bottom)" = extract_bottom(safe_var("OXY_oxy"))),
    ylab = "mmol O2/m\u00b3", title = "Bottom dissolved oxygen",
    colours = c("OXY_oxy (bottom)" = col_bott), hline = 0
  )
  
  p1c <- ts_gg(
    make_ts_df("OXY_oxy_atm" = safe_var("OXY_oxy_atm")),
    ylab = "mmol O2/m\u00b2/d", title = "Atmospheric O2 exchange (areal)",
    colours = c("OXY_oxy_atm" = col_flux), hline = 0
  )
  
  p1d <- ts_gg(
    make_ts_df("SDF_Fsed_oxy" = safe_var("SDF_Fsed_oxy")),
    ylab = "mmol O2/m\u00b2/d", title = "Sediment O2 flux",
    colours = c("SDF_Fsed_oxy" = col_bott), hline = 0
  )
  
  p1e <- ts_gg(
    make_ts_df("wind" = safe_var("wind")),
    ylab = "m/s", title = "Wind speed",
    colours = c("wind" = col_grey)
  )
  
  sst <- safe_var("surface_temp")
  if (is.null(sst)) sst <- extract_surface(safe_var("temp"))
  p1f <- ts_gg(
    make_ts_df("surface_temp" = sst),
    ylab = "\u00b0C", title = "Surface temperature",
    colours = c("surface_temp" = col_surf)
  )
  
  page1 <- patchwork::wrap_plots(p1a, p1b, p1c, p1d, p1e, p1f, ncol = 2) +
    patchwork::plot_annotation(
      title = "Panel 1: Oxygen state & key drivers",
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold")
      )
    )
  
  # ===== Page 2: Mixing & physical structure ==============================
  p2a <- ts_gg(
    make_ts_df("NS" = safe_var("NS")),
    ylab = "count", title = "Number of layers (NS)",
    colours = c("NS" = col_grey)
  )
  
  p2b <- ts_gg(
    make_ts_df("lake_number" = safe_var("lake_number")),
    ylab = "", title = "Lake Number",
    colours = c("lake_number" = col_flux)
  )
  
  p2c <- ts_gg(
    make_ts_df("max_dT_dz" = safe_var("max_dT_dz")),
    ylab = "\u00b0C/m", title = "Max dT/dz (stratification strength)",
    colours = c("max_dT_dz" = col_surf)
  )
  
  heat_df <- make_ts_df(
    "Qsw" = safe_var("daily_qsw"),
    "Qe"  = safe_var("daily_qe"),
    "Qh"  = safe_var("daily_qh"),
    "Qlw" = safe_var("daily_qlw")
  )
  p2d <- ts_gg(heat_df, ylab = "W/m\u00b2", title = "Surface heat fluxes",
               colours = col_heat)
  
  p2e <- ts_gg(
    make_ts_df("CHE" = safe_var("CHE")),
    ylab = "", title = "Bulk transfer coefficient (CHE)",
    colours = c("CHE" = col_grey)
  )
  
  p2f <- ts_gg(
    make_ts_df(
      "OXY_oxy_atmv (surf)" = extract_surface(safe_var("OXY_oxy_atmv"))
    ),
    ylab = "mmol O2/m\u00b3/d",
    title = "Atmospheric O2 flux (volumetric, surface)",
    colours = c("OXY_oxy_atmv (surf)" = col_flux), hline = 0
  )
  
  page2 <- patchwork::wrap_plots(p2a, p2b, p2c, p2d, p2e, p2f, ncol = 2) +
    patchwork::plot_annotation(
      title = "Panel 2: Mixing & physical structure",
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold")
      )
    )
  
  # ===== Page 3: Biological oxygen demand =================================
  p3a <- ts_gg(
    make_ts_df(
      "surface" = extract_surface(safe_var("OGM_doc_min")),
      "bottom"  = extract_bottom(safe_var("OGM_doc_min"))
    ),
    ylab = "mmol C/m\u00b3/d", title = "DOC mineralisation",
    colours = c("surface" = col_surf, "bottom" = col_bott)
  )
  
  p3b <- ts_gg(
    make_ts_df(
      "anaerobic (bottom)" = extract_bottom(safe_var("OGM_doc_anaerobic")),
      "denit (bottom)"     = extract_bottom(safe_var("OGM_doc_denit"))
    ),
    ylab = "mmol C/m\u00b3/d", title = "Anaerobic DOC metabolism (bottom)",
    colours = c("anaerobic (bottom)" = col_bott, "denit (bottom)" = col_flux)
  )
  
  p3c <- ts_gg(
    make_ts_df(
      "cyano"  = extract_surface(safe_var("PHY_cyano_rsp_c")),
      "green"  = extract_surface(safe_var("PHY_green_rsp_c")),
      "diatom" = extract_surface(safe_var("PHY_diatom_rsp_c"))
    ),
    ylab = "mmol C/m\u00b3/d",
    title = "Phyto respiration (surface, by group)",
    colours = col_phyto
  )
  
  p3d <- ts_gg(
    make_ts_df(
      "cyano"  = extract_surface(safe_var("PHY_cyano_gpp_c")),
      "green"  = extract_surface(safe_var("PHY_green_gpp_c")),
      "diatom" = extract_surface(safe_var("PHY_diatom_gpp_c"))
    ),
    ylab = "mmol C/m\u00b3/d",
    title = "Phyto GPP (surface, by group)",
    colours = col_phyto
  )
  
  p3e <- ts_gg(
    make_ts_df(
      "surface" = extract_surface(safe_var("PHY_tphy")),
      "bottom"  = extract_bottom(safe_var("PHY_tphy"))
    ),
    ylab = "mmol/m\u00b3", title = "Total phytoplankton biomass",
    colours = c("surface" = col_surf, "bottom" = col_bott)
  )
  
  p3f <- ts_gg(
    make_ts_df(
      "surface" = extract_surface(safe_var("OGM_bod5")),
      "bottom"  = extract_bottom(safe_var("OGM_bod5"))
    ),
    ylab = "mg O2/L", title = "BOD5",
    colours = c("surface" = col_surf, "bottom" = col_bott)
  )
  
  page3 <- patchwork::wrap_plots(p3a, p3b, p3c, p3d, p3e, p3f, ncol = 2) +
    patchwork::plot_annotation(
      title = "Panel 3: Biological oxygen demand",
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold")
      )
    )
  
  # ===== Page 4: Sediment fluxes ==========================================
  p4a <- ts_gg(
    make_ts_df("SDF_Fsed_oxy" = safe_var("SDF_Fsed_oxy")),
    ylab = "mmol O2/m\u00b2/d", title = "SDF_Fsed_oxy",
    colours = c("SDF_Fsed_oxy" = col_bott), hline = 0
  )
  
  p4b <- ts_gg(
    make_ts_df("SDF_Fsed_amm" = safe_var("SDF_Fsed_amm")),
    ylab = "mmol N/m\u00b2/d", title = "SDF_Fsed_amm",
    colours = c("SDF_Fsed_amm" = col_flux)
  )
  
  p4c <- ts_gg(
    make_ts_df("SDF_Fsed_nit" = safe_var("SDF_Fsed_nit")),
    ylab = "mmol N/m\u00b2/d", title = "SDF_Fsed_nit",
    colours = c("SDF_Fsed_nit" = col_flux)
  )
  
  p4d <- ts_gg(
    make_ts_df("SDF_Fsed_frp" = safe_var("SDF_Fsed_frp")),
    ylab = "mmol P/m\u00b2/d", title = "SDF_Fsed_frp",
    colours = c("SDF_Fsed_frp" = col_flux)
  )
  
  page4 <- patchwork::wrap_plots(p4a, p4b, p4c, p4d, ncol = 2) +
    patchwork::plot_annotation(
      title = "Panel 4: Sediment fluxes",
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold")
      )
    )
  
  # ---- 4. Output ---------------------------------------------------------
  pages <- list(oxy = page1, physical = page2, bod =page3, sediment = page4)
  return(pages)
  
  # if (!is.null(output_dir)) {
  #   dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  #   pdf_file <- file.path(output_dir,
  #                         paste0("glm_diagnostics_", Sys.Date(), ".pdf"))
  #   ggplot2::ggsave(
  #     pdf_file,
  #     gridExtra::marrangeGrob(
  #       grobs = lapply(pages, patchwork::patchworkGrob),
  #       nrow = 1, ncol = 1, top = NULL
  #     ),
  #     width = 14, height = 10
  #   )
  #   cli::cli_alert_success("Saved to {.file {pdf_file}}")
  # } else {
  #   for (pg in pages) print(pg)
  # }
  # 
  # invisible(list(out = out, plots = pages))
}
