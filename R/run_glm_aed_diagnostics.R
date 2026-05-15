#' Run GLM-AED diagnostics
#'
#' @inheritParams read_model_outputs
#' @param groups         character vector selecting catalogue entries.
#'                       Accepts:
#'                         - catalogue entry names (e.g. "nitrogen_state"),
#'                         - element codes ("O","N","P","Phy"),
#'                         - types ("state","process").
#'                       Default NULL = all entries.
#' @param depth_collapse "mean", "surface" or "max" — reduce 3D variables
#' @param plot           draw combined plots, grouped by element
#' @param use_bounds     add dashed lines to plots showing expected bounds 
#'  (from catalogue)
#' @param print_table    print the kable summary
#'
#' @export
#' @return  invisibly, list(summary, plots, data)
#' @importFrom knitr kable
#' @importFrom patchwork plot_annotation
#' @import ggplot2
#' @import dplyr
#' @importFrom stats quantile median sd
#' @importFrom tibble tibble
#' @importFrom utils head
#' @importFrom cli cli_inform col_grey col_red
#' @examples
#' aeme_dir <- system.file("extdata/lake/", package = "AEME")
#' # Copy files from package into tempdir
#' path <- tempdir()
#' aeme <- yaml_to_aeme(path = aeme_dir, "aeme.yaml")
#' vars_sim <- c("HYD_strat", "HYD_temp", "HYD_thmcln", "HYD_schstb", 
#'               "CHM_oxycln", "CHM_oxynal",
#'               "NIT_tn", "PHS_tp", "PHY_tchla")
#' model_controls <- get_model_controls(use_bgc = TRUE)
#' model_controls <- set_vars_sim(model_controls = model_controls,
#'                                vars_sim = vars_sim)
#' model <- c("glm_aed")
#' aeme <- build_aeme(path = path, aeme = aeme, model = model,
#'                    model_controls = model_controls,
#'                    ext_elev = 5, use_bgc = TRUE)
#' 
#' aeme <- run_aeme(aeme)
#' out <- run_glm_aed_diagnostics(aeme = aeme)

run_glm_aed_diagnostics <- function(aeme,
                                    model,
                                    groups         = NULL,
                                    depth_collapse = "mean",
                                    plot           = TRUE,
                                    use_bounds = TRUE,
                                    print_table    = TRUE) {
  
  lake_dir <- get_lake_dir(aeme)
  if (missing(model)) {
    model <- list_models(aeme)
  }
  
  # Get sed zones 
  cfg <- read_model_config(model = "glm_aed", lake_dir = lake_dir)
  n_zones <- cfg[["hydrodynamic"]][["sediment"]][["n_zones"]]
  H <- cfg[["hydrodynamic"]][["morphometry"]][["H"]]
  if (n_zones > 1) {
    zone_heights <- cfg[["hydrodynamic"]][["sediment"]][["zone_heights"]]
  } else {
    zone_heights <- max(H) - min(H)  # single zone = whole depth
  }
  
  # --- select groups -------------------------------------------------------
  all_groups <- glm_aed_diag_catalogue
  if (!is.null(groups)) {
    if (all(groups %in% names(all_groups))) {
      sel <- all_groups[groups]
    } else if (all(groups %in% c("O", "N", "P", "Phy", "Sed"))) {
      sel <- all_groups[vapply(all_groups, `[[`, "", "element") %in% groups]
    } else if (all(groups %in% c("state", "process"))) {
      sel <- all_groups[vapply(all_groups, `[[`, "", "type") %in% groups]
    } else {
      stop("`groups` must be catalogue entry names, element codes ",
           "(O/N/P/Phy), or types (state/process).")
    }
  } else {
    sel <- all_groups
  }
  
  # Prepare data frame with variable and label
  var_label <- lapply(sel, \(x) {
    data.frame(variable = names(x$vars), label = unname(x$vars))
  }) |> 
    dplyr::bind_rows()
  
  # --- fetch data ----------------------------------------------------------
  req_vars <- unique(unlist(lapply(sel, function(g) names(g$vars))))
  cli::cli_inform("Requesting {length(req_vars)} variables from model output...
                  {cli::col_grey(' (sediment zone _Z variables are optional; 
                  missing ones are skipped)')}")
  
  raw <- read_model_outputs(lake_dir = lake_dir, model = model,
                            vars_sim = req_vars)
  lake_bed_elevation <- min(H)
  dat <- .tidy_model_output(raw,
                            zone_heights       = zone_heights,
                            lake_bed_elevation = lake_bed_elevation) |>
    .collapse_depth(depth_collapse) |> 
    dplyr::left_join(var_label, by = "variable")
  
  got     <- unique(dat$variable)
  missing <- setdiff(req_vars, got)
  if (length(missing)) {
    cli::cli_inform("  ({length(missing)} variables not in output: 
                    {cli::col_red(paste(utils::head(missing, 6), collapse = ', '))} 
                    {if (length(missing) > 6) cli::col_red(', ...') else ''})")
  }
  
  # --- summarise -----------------------------------------------------------
  summaries <- lapply(names(sel), function(nm) {
    s <- summarise_diag_group(dat, sel[[nm]])
    if (!is.null(s)) s$group <- nm
    s
  })
  summary_tbl <- dplyr::bind_rows(summaries) |>
    dplyr::relocate(group)
  
  # --- plots ---------------------------------------------------------------
  plots        <- lapply(sel, function(g) {
    plot_diag_group(dat, group = g, use_bounds = use_bounds)
  })
  names(plots) <- names(sel)
  
  # --- output --------------------------------------------------------------
  if (print_table) {
    cat("\n=== GLM-AED diagnostic summary ===\n")
    flagged <- dplyr::filter(summary_tbl, flag != "ok")
    if (nrow(flagged)) {
      cat("\n** FLAGGED VARIABLES **\n")
      print(knitr::kable(
        flagged |>
          dplyr::select(group, variable, flag, min, max, n_below, n_above),
        digits = 3))
    } else {
      cat("All diagnostics within expected ranges.\n")
    }
    cat("\n-- Full summary --\n")
    print(knitr::kable(
      summary_tbl |>
        dplyr::select(group, variable, label, min, median, mean, max, sd, flag),
      digits = 3))
  }
  
  if (plot) {
    # Combine plots by element so we don't get a single 50-panel monster
    by_elem <- split(names(sel), vapply(sel, `[[`, "", "element"))
    for (el in names(by_elem)) {
      ps <- plots[by_elem[[el]]]
      combined <- Reduce(`/`, ps) +
        patchwork::plot_annotation(
          title = paste("GLM-AED diagnostics -", el),
          theme = ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")))
      print(combined)
    }
  }
  
  invisible(list(summary = summary_tbl, plots = plots, data = dat))
}


glm_aed_diag_catalogue <- list(
  
  # ---------------- OXYGEN ----------------
  oxygen_state = list(
    label   = "Oxygen - concentration & saturation",
    element = "O",
    type    = "state",
    vars    = c(OXY_oxy = "O2 (mmol/m3)",
                OXY_sat = "O2 saturation (%)"),
    bounds  = list(OXY_oxy = c(0, 600),
                   OXY_sat = c(0, 200))
  ),
  oxygen_fluxes = list(
    label   = "Oxygen - air/sediment fluxes",
    element = "O",
    type    = "process",
    vars    = c(OXY_oxy_atm  = "Atm O2 flux (mmol/m2/d)",
                OXY_oxy_dsf  = "SWI O2 flux (mmol/m2/d)",
                OXY_oxy_atmv = "Atm O2 flux (vol)",
                OXY_oxy_dsfv = "SOD (vol)"),
    bounds  = list(OXY_oxy_atm = c(-500, 500),
                   OXY_oxy_dsf = c(-500, 100))
  ),
  
  # ---------------- NITROGEN ----------------
  nitrogen_state = list(
    label   = "Nitrogen - dissolved inorganic pools",
    element = "N",
    type    = "state",
    vars    = c(NIT_amm = "NH4 (mmol N/m3)",
                NIT_nit = "NO3 (mmol N/m3)",
                NIT_no2 = "NO2 (mmol N/m3)",
                NIT_n2o = "N2O (mmol N/m3)"),
    bounds  = list(NIT_amm = c(0, 500),
                   NIT_nit = c(0, 500),
                   NIT_no2 = c(0, 100),
                   NIT_n2o = c(0, 100))
  ),
  nitrogen_organic = list(
    label   = "Nitrogen - organic pools",
    element = "N",
    type    = "state",
    vars    = c(OGM_don  = "DON (mmol N/m3)",
                OGM_pon  = "PON (mmol N/m3)",
                OGM_donr = "Refractory DON"),
    bounds  = list(OGM_don = c(0, 500),
                   OGM_pon = c(0, 500))
  ),
  nitrogen_transformations = list(
    label   = "Nitrogen - transformation rates",
    element = "N",
    type    = "process",
    vars    = c(NIT_nitrif  = "Nitrification",
                NIT_denit   = "Denitrification",
                NIT_anammox = "Anammox",
                NIT_dnra    = "DNRA",
                NIT_n2oprod = "N2O production"),
    bounds  = list(NIT_nitrif  = c(0, 200),
                   NIT_denit   = c(0, 200),
                   NIT_anammox = c(0, 200),
                   NIT_dnra    = c(0, 200))
  ),
  nitrogen_sediment_flux = list(
    label   = "Nitrogen - sediment/atmosphere fluxes",
    element = "N",
    type    = "process",
    vars    = c(NIT_amm_dsf = "NH4 SWI flux",
                NIT_nit_dsf = "NO3 SWI flux",
                NIT_n2o_dsf = "N2O SWI flux",
                NIT_no2_dsf = "NO2 SWI flux",
                NIT_n2o_atm = "N2O atm flux")
  ),
  
  # ---------------- PHOSPHORUS ----------------
  phosphorus_state = list(
    label   = "Phosphorus - dissolved & organic pools",
    element = "P",
    type    = "state",
    vars    = c(PHS_frp  = "FRP (mmol P/m3)",
                OGM_dop  = "DOP",
                OGM_pop  = "POP",
                OGM_dopr = "Refractory DOP"),
    bounds  = list(PHS_frp = c(0, 100),
                   OGM_dop = c(0, 100),
                   OGM_pop = c(0, 100))
  ),
  phosphorus_fluxes = list(
    label   = "Phosphorus - sediment fluxes & cycling",
    element = "P",
    type    = "process",
    vars    = c(PHS_frp_dsf = "FRP SWI flux",
                OGM_dop_swi = "DOP SWI flux",
                OGM_pop_swi = "POP SWI flux",
                OGM_pop_res = "POP resuspension",
                OGM_dop_min = "DOP mineralisation"),
    bounds  = list(
      PHS_frp_dsf = c(-10,  10),   # FRP: small release (+) or uptake (-)
      OGM_dop_swi = c(-10,  10),   # DOP SWI: typically small bidirectional
      OGM_pop_swi = c(-50,  10),   # POP settling is negative; resuspension positive
      OGM_pop_res = c(  0,  50),   # resuspension always >= 0
      OGM_dop_min = c(  0,   5)    # mineralisation always >= 0, rate rarely large
    )
  ),
  
  # ---------------- PHYTOPLANKTON ----------------
  phyto_biomass = list(
    label   = "Phytoplankton - biomass",
    element = "Phy",
    type    = "state",
    vars    = c(PHY_tphy   = "Total phyto (mmol C/m3)",
                PHY_tchla  = "Total chl-a (ug/L)",
                PHY_cyano  = "Cyanobacteria",
                PHY_green  = "Greens",
                PHY_diatom = "Diatoms"),
    bounds  = list(PHY_tchla  = c(0, 500),
                   PHY_tphy   = c(0, 2000),
                   PHY_cyano  = c(0, 2000),
                   PHY_green  = c(0, 2000),
                   PHY_diatom = c(0, 2000))
  ),
  phyto_stoichiometry = list(
    label   = "Phytoplankton - internal stoichiometry",
    element = "Phy",
    type    = "state",
    vars    = c(PHY_cyano_NtoP  = "Cyano N:P",
                PHY_green_NtoP  = "Green N:P",
                PHY_diatom_NtoP = "Diatom N:P"),
    # Redfield = 16, so flag extreme departures
    bounds  = list(PHY_cyano_NtoP  = c(1, 100),
                   PHY_green_NtoP  = c(1, 100),
                   PHY_diatom_NtoP = c(1, 100))
  ),
  phyto_limitation = list(
    label   = "Phytoplankton - growth limitation factors (0-1)",
    element = "Phy",
    type    = "process",
    vars    = c(PHY_cyano_fI    = "Cyano fI",
                PHY_cyano_fNit  = "Cyano fN",
                PHY_cyano_fPho  = "Cyano fP",
                PHY_cyano_fT    = "Cyano fT",
                PHY_green_fI    = "Green fI",
                PHY_green_fNit  = "Green fN",
                PHY_green_fPho  = "Green fP",
                PHY_green_fT    = "Green fT",
                PHY_diatom_fI   = "Diatom fI",
                PHY_diatom_fNit = "Diatom fN",
                PHY_diatom_fPho = "Diatom fP",
                PHY_diatom_fT   = "Diatom fT"),
    # Limitation factors should be in [0,1]; fT can exceed 1
    bounds  = list(PHY_cyano_fI    = c(0, 1.01),
                   PHY_cyano_fNit  = c(0, 1.01),
                   PHY_cyano_fPho  = c(0, 1.01),
                   PHY_green_fI    = c(0, 1.01),
                   PHY_green_fNit  = c(0, 1.01),
                   PHY_green_fPho  = c(0, 1.01),
                   PHY_diatom_fI   = c(0, 1.01),
                   PHY_diatom_fNit = c(0, 1.01),
                   PHY_diatom_fPho = c(0, 1.01))
  ),
  phyto_fluxes = list(
    label   = "Phytoplankton - community rates",
    element = "Phy",
    type    = "process",
    vars    = c(PHY_gpp     = "GPP",
                PHY_ncp     = "NCP",
                PHY_upt_no3 = "NO3 uptake",
                PHY_upt_nh4 = "NH4 uptake",
                PHY_upt_po4 = "PO4 uptake",
                PHY_set     = "Sedimentation"),
    bounds  = list(
      PHY_gpp     = c(   0, 50),   # GPP always >= 0
      PHY_ncp     = c( -10, 50),   # NCP can be slightly negative (respiration > GPP)
      PHY_upt_no3 = c(   0, 20),   # uptake rates always >= 0
      PHY_upt_nh4 = c(   0, 20),
      PHY_upt_po4 = c(   0,  5),
      PHY_set     = c( -30,  5)    # negative = downward; flag if < -30
    )
  ),
  
  # ---------------- SEDIMENT ZONES (_Z vars) ----------------
  # Only present when the AED sediment flux / diagenesis module writes
  # per-zone output. Variables are [n_zones x time] matrices — each zone
  # index is treated like a depth layer and collapsed by depth_collapse.
  # Use groups = "Sed" to run just these panels.
  
  sedflux_oxygen_Z = list(
    label   = "Sed. zones - O2 fluxes",
    element = "Sed",
    type    = "process",
    vars    = c(SDF_Fsed_oxy_Z = "SDF O2 flux (per zone)",
                OXY_oxy_dsf_Z  = "SWI O2 exchange (per zone)",
                OXY_oxy_atm_Z  = "Atm O2 flux (per zone)"),
    bounds  = list(SDF_Fsed_oxy_Z = c(-500, 100),
                   OXY_oxy_dsf_Z  = c(-500, 100))
  ),
  sedflux_nitrogen_Z = list(
    label   = "Sed. zones - N fluxes",
    element = "Sed",
    type    = "process",
    vars    = c(SDF_Fsed_amm_Z = "SDF NH4 flux (per zone)",
                SDF_Fsed_nit_Z = "SDF NO3 flux (per zone)",
                NIT_amm_dsf_Z  = "NH4 SWI flux (per zone)",
                NIT_nit_dsf_Z  = "NO3 SWI flux (per zone)",
                NIT_n2o_dsf_Z  = "N2O SWI flux (per zone)",
                NIT_no2_dsf_Z  = "NO2 SWI flux (per zone)",
                NIT_n2o_atm_Z  = "N2O atm flux (per zone)"),
    bounds  = list(SDF_Fsed_amm_Z = c(-50,  50),
                   SDF_Fsed_nit_Z = c(-50,  50),
                   NIT_amm_dsf_Z  = c(-50,  50),
                   NIT_nit_dsf_Z  = c(-50,  50))
  ),
  sedflux_phosphorus_Z = list(
    label   = "Sed. zones - P fluxes",
    element = "Sed",
    type    = "process",
    vars    = c(SDF_Fsed_frp_Z  = "SDF FRP flux (per zone)",
                PHS_frp_dsf_Z   = "FRP SWI flux (per zone)",
                OGM_poc_swi_Z   = "POC SWI flux (per zone)",
                OGM_doc_swi_Z   = "DOC SWI flux (per zone)",
                OGM_pon_swi_Z   = "PON SWI flux (per zone)",
                OGM_don_swi_Z   = "DON SWI flux (per zone)",
                OGM_pop_swi_Z   = "POP SWI flux (per zone)",
                OGM_dop_swi_Z   = "DOP SWI flux (per zone)"),
    bounds  = list(SDF_Fsed_frp_Z = c(-20,  20),
                   PHS_frp_dsf_Z  = c(-20,  20),
                   OGM_pop_swi_Z  = c(-50,  10),
                   OGM_dop_swi_Z  = c(-10,  10))
  ),
  sedflux_organic_Z = list(
    label   = "Sed. zones - organic sediment stocks",
    element = "Sed",
    type    = "state",
    vars    = c(OGM_toc_sed_Z   = "TOC sed mass (per zone)",
                OGM_ton_sed_Z   = "TON sed mass (per zone)",
                OGM_top_sed_Z   = "TOP sed mass (per zone)",
                OGM_poc_res_Z   = "POC resuspension (per zone)",
                OGM_pon_res_Z   = "PON resuspension (per zone)",
                OGM_pop_res_Z   = "POP resuspension (per zone)",
                PHY_phy_swi_c_Z = "Phyto SWI C (per zone)",
                PHY_phy_swi_n_Z = "Phyto SWI N (per zone)",
                PHY_phy_swi_p_Z = "Phyto SWI P (per zone)"),
    bounds  = list(OGM_toc_sed_Z = c(0, 1e6),
                   OGM_ton_sed_Z = c(0, 1e5),
                   OGM_top_sed_Z = c(0, 1e4),
                   OGM_poc_res_Z = c(0,  500),
                   OGM_pon_res_Z = c(0,  500),
                   OGM_pop_res_Z = c(0,  100))
  ),
  sedflux_silica_Z = list(
    label   = "Sed. zones - Si fluxes",
    element = "Sed",
    type    = "process",
    vars    = c(SIL_dsf_rsi_Z = "Si SWI flux (per zone)"),
    bounds  = list(SIL_dsf_rsi_Z = c(-50, 50))
  )
)


# -----------------------------------------------------------------------------
# 2. HELPERS
# -----------------------------------------------------------------------------

# Coerce what read_model_outputs() returns into a long tibble with columns:
#   DateTime, depth (m from surface; NA for 1D and _Z vars),
#   zone     (character label; set for _Z vars always, and for water-column
#              vars when zone_heights + lake_bed_elevation are supplied),
#   variable, value.
#
# Coordinate system
# -----------------
# LKE_depths    : depth below the instantaneous water surface (m, positive down)
# LKE_lvlwtr   : absolute water surface elevation at each timestep (same datum as H)
# H (morphometry): absolute elevations (same datum); min(H) = lake bed elevation
# zone_heights  : upper boundaries of sediment zones measured as height above
#                 the lake BED (m).  e.g. c(3.5, 9) ->
#                   Zone 1: 0 – 3.5 m above bed  (row 1 of _Z matrices)
#                   Zone 2: 3.5 – 9 m above bed  (row 2 of _Z matrices)
#
# For water-column layers the height above bed at timestep t is:
#   LKE_lvlwtr[t] - LKE_depths[layer, t] - lake_bed_elevation
# This is compared against zone_heights to assign each layer to a zone,
# accounting for the changing lake level at every timestep.
#
# Arguments
# ---------
# x                  : list returned by read_model_outputs()
# zone_heights       : numeric vector of zone upper boundaries (m above bed)
# lake_bed_elevation : min(H) from the GLM morphometry config

#' @noRd
.tidy_model_output <- function(x,
                               zone_heights       = NULL,
                               lake_bed_elevation = NULL) {
  
  if (!is.list(x))
    stop("read_model_outputs() must return a list; got ", class(x)[1])
  
  # --- time axis ------------------------------------------------------------
  time_name <- intersect(c("Date", "DateTime", "datetime", "time", "date"),
                         names(x))[1]
  if (is.na(time_name))
    stop("No time element found (looked for Date / DateTime / time).")
  
  times <- as.POSIXct(x[[time_name]], tz = "UTC")
  nt    <- length(times)
  
  # --- water-column depth matrix and lake level vector ----------------------
  depth_name <- intersect(c("LKE_depths", "depths", "z"), names(x))[1]
  depth_mat  <- if (!is.na(depth_name)) x[[depth_name]] else NULL
  
  # LKE_lvlwtr: water surface elevation, length nt
  lvl_vec <- if ("LKE_lvlwtr" %in% names(x)) as.numeric(x[["LKE_lvlwtr"]]) else NULL
  
  # --- zone label builder ---------------------------------------------------
  .make_zone_labels <- function(zh) {
    lo <- c(0, zh[-length(zh)])
    sprintf("Zone %d (%g\u2013%g m)", seq_along(zh), lo, zh)
  }
  zone_labels <- if (!is.null(zone_heights)) .make_zone_labels(zone_heights) else NULL
  n_zones_cfg <- length(zone_labels)
  
  # --- assign_zone_matrix: [nz x nt] depth matrix -> [nz x nt] zone labels --
  # height_from_bed[layer, t] = lvlwtr[t] - depth[layer, t] - lake_bed_elevation
  # We then find which zone interval each height falls into.
  # Layers above the top zone boundary are assigned to the topmost zone.
  # Layers with NA depth (inactive GLM layers) get NA zone.
  .assign_zone_matrix <- function(dep_mat, lvl, bed_elev, zh, zlabels) {
    nz <- nrow(dep_mat)
    nt <- ncol(dep_mat)
    # height above bed for every [layer, time] combination
    # lvl is length nt so sweep across columns
    hfb <- sweep(-dep_mat, 2, lvl, `+`) - bed_elev   # [nz x nt]
    # For each value find the lowest zone boundary it does not exceed
    # zone_heights are upper bounds: zone i if hfb <= zh[i]
    zone_idx <- matrix(NA_integer_, nz, nt)
    for (i in seq_along(zh)) {
      # assign zone i to layers not yet assigned AND within this boundary
      unassigned <- is.na(zone_idx)
      within     <- !is.na(hfb) & hfb <= zh[i]
      zone_idx[unassigned & within] <- i
    }
    # anything still unassigned is above the top zone -> assign to topmost zone
    zone_idx[is.na(zone_idx) & !is.na(hfb)] <- n_zones_cfg
    # convert index matrix to label matrix
    lbl_mat <- matrix(NA_character_, nz, nt)
    for (i in seq_along(zh)) lbl_mat[zone_idx == i] <- zlabels[i]
    lbl_mat
  }
  
  can_assign_wc_zones <- !is.null(zone_heights) &&
    !is.null(lake_bed_elevation) &&
    !is.null(lvl_vec) &&
    !is.null(depth_mat)
  
  # --- candidate variables --------------------------------------------------
  drop_names <- c(time_name, depth_name, "LKE_lvlwtr",
                  "ok", "reason", "status")
  cand <- setdiff(names(x), drop_names)
  
  # Pre-compute zone label matrix for water-column vars (done once, reused)
  wc_zone_mat <- if (can_assign_wc_zones) {
    .assign_zone_matrix(depth_mat, lvl_vec, lake_bed_elevation,
                        zone_heights, zone_labels)
  } else NULL
  
  rows <- list()
  for (nm in cand) {
    v <- x[[nm]]
    if (!is.numeric(v)) next
    
    is_Z_var <- grepl("_Z$", nm)
    
    # 1D time series --------------------------------------------------------
    if (is.null(dim(v)) || length(dim(v)) == 1L || any(dim(v) == 1L)) {
      vv <- as.numeric(v)
      if (length(vv) != nt) next
      rows[[nm]] <- tibble::tibble(
        DateTime = times,
        depth    = NA_real_,
        zone     = NA_character_,
        variable = nm,
        value    = vv
      )
      next
    }
    
    # 2D matrix [nrows x nt] ------------------------------------------------
    if (is.matrix(v) && ncol(v) == nt) {
      nz <- nrow(v)
      
      if (is_Z_var) {
        # Sediment zone variable: row i -> zone i.
        #
        # GLM sometimes writes _Z matrices with more rows than configured zones
        # (e.g. nz = n_zones_cfg + 1) where the extra row(s) are entirely NA.
        # Detect and drop trailing all-NA rows so phantom zones don't appear.
        trailing_na_rows <- 0L
        if (nz > 1L) {
          for (row_i in rev(seq_len(nz))) {
            if (all(is.na(v[row_i, ]))) {
              trailing_na_rows <- trailing_na_rows + 1L
            } else break
          }
        }
        if (trailing_na_rows > 0L) {
          keep_rows <- seq_len(nz - trailing_na_rows)
          v  <- v[keep_rows, , drop = FALSE]
          nz <- nrow(v)
        }
        
        if (!is.null(zone_labels) && nz == n_zones_cfg) {
          zlabels <- zone_labels
        } else {
          zlabels <- sprintf("Zone %d", seq_len(nz))
          if (!is.null(zone_heights) && nz != n_zones_cfg)
            message(nm, ": matrix has ", nz, " active rows; zone_heights implies ",
                    n_zones_cfg, " zones — using integer labels for this variable.")
        }
        rows[[nm]] <- tibble::tibble(
          DateTime = rep(times, each = nz),
          depth    = NA_real_,
          zone     = rep(zlabels, times = nt),
          variable = nm,
          value    = as.numeric(v)
        )
        
      } else {
        # Water-column variable
        if (!is.null(depth_mat) && all(dim(depth_mat) == dim(v))) {
          d <- as.numeric(depth_mat)
        } else {
          d <- rep(seq_len(nz), times = nt)
        }
        # Assign zone labels from the pre-computed matrix if available
        z <- if (!is.null(wc_zone_mat) && all(dim(wc_zone_mat) == dim(v))) {
          as.character(wc_zone_mat)   # already [nz x nt], unroll column-major
        } else {
          NA_character_
        }
        rows[[nm]] <- tibble::tibble(
          DateTime = rep(times, each = nz),
          depth    = d,
          zone     = z,
          variable = nm,
          value    = as.numeric(v)
        )
      }
      next
    }
    # anything else -> skip silently
  }
  
  if (!length(rows))
    stop("No usable numeric variables found in model output.")
  
  dplyr::bind_rows(rows)
}

# Collapse a long tidy df to one row per (DateTime, variable) — or, when
# collapse = "zone", one row per (DateTime, variable, zone).
#
# Streams:
#   pure1D : no depth, no zone  -> pass through unchanged
#   wc     : has depth          -> collapse layers, optionally by zone
#   zn     : has zone, no depth -> _Z vars; already one row per zone per time,
#                                  pass through unchanged (zone label retained)
#
# collapse options
# ----------------
#   "mean"    average across layers (water-column) / not applicable to _Z
#   "surface" value at shallowest layer (smallest LKE_depth value)
#   "max"     maximum across layers
#   "zone"    for water-column vars: average within each zone (requires zone
#             column populated by .tidy_model_output); _Z vars pass through as-is
#
# In all cases the returned tibble has columns: DateTime, variable, zone, value.
# zone is NA except when collapse = "zone" (or for _Z vars which always have it).
#' @noRd
.collapse_depth <- function(df, collapse = c("mean", "surface", "max", "zone")) {
  collapse <- match.arg(collapse)
  
  # Separate the three streams
  pure1D <- dplyr::filter(df,  is.na(depth) &  is.na(zone))
  wc     <- dplyr::filter(df, !is.na(depth))          # water-column layers
  zn     <- dplyr::filter(df,  is.na(depth) & !is.na(zone))  # _Z vars
  
  # Ensure all have a zone column (pure1D and non-zone wc rows may lack it)
  if (!"zone" %in% names(pure1D)) pure1D$zone <- NA_character_
  if (!"zone" %in% names(wc))     wc$zone     <- NA_character_
  
  # --- water-column collapse ------------------------------------------------
  if (nrow(wc)) {
    if (collapse == "zone") {
      # Check zone labels are present; warn if not (zone_heights not supplied)
      if (all(is.na(wc$zone))) {
        warning(".collapse_depth(collapse='zone'): water-column zone labels are ",
                "all NA — zone_heights and lake_bed_elevation must be passed to ",
                ".tidy_model_output(). Falling back to overall mean.")
        wc_c <- wc |>
          dplyr::group_by(DateTime, variable) |>
          dplyr::summarise(zone = NA_character_,
                           value = mean(value, na.rm = TRUE), .groups = "drop")
      } else {
        # Average within each zone at each timestep
        wc_c <- wc |>
          dplyr::filter(!is.na(zone)) |>
          dplyr::group_by(DateTime, variable, zone) |>
          dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
      }
    } else {
      fn <- switch(collapse,
                   mean    = function(v, d) mean(v, na.rm = TRUE),
                   surface = function(v, d) v[which.min(d)],
                   max     = function(v, d) max(v, na.rm = TRUE))
      wc_c <- wc |>
        dplyr::group_by(DateTime, variable) |>
        dplyr::summarise(zone  = NA_character_,
                         value = fn(value, depth), .groups = "drop")
    }
  } else {
    wc_c <- tibble::tibble(DateTime = as.POSIXct(character()),
                           variable = character(),
                           zone     = character(),
                           value    = numeric())
  }
  
  # --- _Z vars: always pass through with zone label intact ------------------
  zn_c <- if (nrow(zn)) {
    dplyr::select(zn, DateTime, variable, zone, value)
  } else {
    tibble::tibble(DateTime = as.POSIXct(character()),
                   variable = character(),
                   zone     = character(),
                   value    = numeric())
  }
  
  # --- combine --------------------------------------------------------------
  pure1D$zone <- NA_character_
  dplyr::bind_rows(
    dplyr::select(pure1D, DateTime, variable, zone, value),
    wc_c,
    zn_c
  )
}


# -----------------------------------------------------------------------------
# 3. CORE SUMMARY
# -----------------------------------------------------------------------------

#' Summarise diagnostic variables for one catalogue group
#' @noRd
summarise_diag_group <- function(data, group) {
  
  vars_present <- intersect(names(group$vars), unique(data$variable))
  if (!length(vars_present)) return(NULL)
  
  bounds <- group$bounds %||% list()
  
  # Pre-compute bound violations per variable (vectorised, no rowwise joins)
  viol <- lapply(vars_present, function(v) {
    vals  <- data$value[data$variable == v]
    lower <- bounds[[v]][1] %||% NA_real_
    upper <- bounds[[v]][2] %||% NA_real_
    tibble::tibble(
      variable = v,
      lower    = lower,
      upper    = upper,
      n_below  = if (is.na(lower)) NA_integer_
      else sum(vals < lower, na.rm = TRUE),
      n_above  = if (is.na(upper)) NA_integer_
      else sum(vals > upper, na.rm = TRUE)
    )
  }) |> dplyr::bind_rows()
  
  stats_df <- data |>
    dplyr::filter(variable %in% vars_present) |>
    dplyr::group_by(variable) |>
    dplyr::summarise(
      n      = sum(!is.na(value)),
      n_na   = sum(is.na(value)),
      n_neg  = sum(value < 0, na.rm = TRUE),
      min    = suppressWarnings(min(value, na.rm = TRUE)),
      p05    = stats::quantile(value, 0.05, na.rm = TRUE),
      median = stats::median(value, na.rm = TRUE),
      mean   = mean(value, na.rm = TRUE),
      p95    = stats::quantile(value, 0.95, na.rm = TRUE),
      max    = suppressWarnings(max(value, na.rm = TRUE)),
      sd     = stats::sd(value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(viol, by = "variable") |>
    dplyr::mutate(
      label = unname(group$vars[variable]),
      flag  = dplyr::case_when(
        !is.finite(max) | !is.finite(min)                      ~ "NO DATA",
        (!is.na(n_above) & n_above > 0) |
          (!is.na(n_below) & n_below > 0)                      ~ "OUT OF RANGE",
        # Negative flag: exclude variables where negative values are expected
        # (fluxes, sedimentation, SWI exchange, NCP) and _Z counterparts
        n_neg > 0 & !grepl("flux|atm|dsf|ncp|_swi|_set|_res|_Z$",
                           variable, ignore.case = TRUE)       ~ "NEGATIVE",
        # MISSING flag: skip _Z vars (per-zone row counts differ from 1D vars;
        # phantom NA rows are stripped at parse time so n_na should be 0 for
        # active zones — if n == 0 they will already be caught by NO DATA above)
        !grepl("_Z$", variable) &
          n_na / pmax(n + n_na, 1) > 0.1                       ~ "MISSING > 10%",
        TRUE                                                   ~ "ok"
      )
    ) |>
    dplyr::select(variable, label, n, n_na, min, p05, median, mean,
                  p95, max, sd, lower, upper, n_below, n_above, flag)
  
  stats_df
}


# -----------------------------------------------------------------------------
# 4. CORE PLOT
# -----------------------------------------------------------------------------
#' @noRd
plot_diag_group <- function(data, group, free_y = TRUE, use_bounds = TRUE) {
  
  vars_present <- intersect(names(group$vars), unique(data$variable))
  if (!length(vars_present)) {
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 0, y = 0,
                               label = paste("no data for", group$label)) +
             ggplot2::theme_void())
  }
  
  df <- data |>
    dplyr::filter(variable %in% vars_present) |>
    dplyr::mutate(label = factor(unname(group$vars[variable]),
                                 levels = unname(group$vars[vars_present])))
  
  bnd    <- group$bounds %||% list()
  bnd_df <- tibble::tibble(
    variable = names(bnd),
    lower    = vapply(bnd, `[`, numeric(1), 1),
    upper    = vapply(bnd, `[`, numeric(1), 2)
  )
  
  # Colour by zone when the data contains zone labels (either _Z vars or
  # water-column vars collapsed by zone). A single NA zone = no colouring.
  has_zones <- "zone" %in% names(df) && any(!is.na(df$zone))
  
  if (has_zones) {
    p <- ggplot2::ggplot(df, ggplot2::aes(DateTime, value,
                                          colour = zone, group = zone)) +
      ggplot2::geom_line(linewidth = 0.45) +
      ggplot2::scale_colour_brewer(palette = "Set2", name = "Zone") +
      ggplot2::facet_wrap(~ label, scales = if (free_y) "free_y" else "fixed") +
      ggplot2::labs(title = group$label, x = NULL, y = NULL) +
      ggplot2::theme_bw(base_size = 10) +
      ggplot2::theme(strip.background = ggplot2::element_rect(fill = "grey92",
                                                              colour = NA),
                     panel.grid.minor = ggplot2::element_blank(),
                     legend.position  = "bottom")
  } else {
    p <- ggplot2::ggplot(df, ggplot2::aes(DateTime, value)) +
      ggplot2::geom_line(colour = "steelblue4", linewidth = 0.35) +
      ggplot2::facet_wrap(~ label, scales = if (free_y) "free_y" else "fixed") +
      ggplot2::labs(title = group$label, x = NULL, y = NULL) +
      ggplot2::theme_bw(base_size = 10) +
      ggplot2::theme(strip.background = ggplot2::element_rect(fill = "grey92",
                                                              colour = NA),
                     panel.grid.minor = ggplot2::element_blank())
  }
  
  if (nrow(bnd_df) & use_bounds) {
    bnd_df <- bnd_df |> 
      dplyr::filter(variable %in% vars_present) |>
      dplyr::mutate(label = factor(unname(group$vars[variable]),
                                   levels = levels(df$label)))
    
    p <- p +
      ggplot2::geom_hline(data = bnd_df, ggplot2::aes(yintercept = lower),
                          linetype = "dashed", colour = "firebrick",
                          linewidth = 0.3, na.rm = TRUE) +
      ggplot2::geom_hline(data = bnd_df, ggplot2::aes(yintercept = upper),
                          linetype = "dashed", colour = "firebrick",
                          linewidth = 0.3, na.rm = TRUE)
  }
  p
}
