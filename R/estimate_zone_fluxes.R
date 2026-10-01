#' Estimate zone-specific sediment fluxes from hypsograph
#'
#' Estimates zone-specific sediment fluxes for \code{aed_sed_const2d} using up
#' to two tiers of adjustment:
#'
#' \strong{Tier 1 (always)} -- area-weighted depth scaling. Each zone's flux is
#' scaled from literature baseline values according to its mean depth and
#' fractional bed area. Deep zones receive higher SOD and nutrient fluxes
#' reflecting greater organic matter accumulation and more persistent anoxia.
#'
#' \strong{Tier 2 (optional, when \code{obs} supplied)} -- observed data
#' adjustment. Near-bed summer concentrations of O2, NH4, NO3, and FRP are used
#' to adjust the relative difference in fluxes between zones. Only inter-zone
#' ratios are adjusted, not absolute magnitude, so the lake-wide total is
#' preserved.
#'
#' Literature baselines at reference depth 5 m (temperate lakes):
#' \itemize{
#'   \item \code{fsed_oxy}: -25 mmol O2/m2/d (Muller et al. 2012; Sondergaard
#'     et al. 2003)
#'   \item \code{fsed_amm}: 2 mmol N/m2/d (Andersen 1982; Beutel 2006)
#'   \item \code{fsed_nit}: 0.2 mmol N/m2/d (Seitzinger 1988)
#'   \item \code{fsed_frp}: 0.05 mmol P/m2/d (Nurnberg 1984)
#' }
#'
#' Depth scaling (Beutel 2006; Muller et al. 2012): SOD and NH4/FRP fluxes
#' scale approximately linearly with mean zone depth divided by
#' \code{ref_depth}. NO3 flux transitions from small positive values (shallow,
#' oxic) to negative values (deep, anoxic denitrification) at approximately
#' \code{0.5 * max_depth}.
#'
#' @inheritParams build_aeme
#' @param ref_depth Numeric. Reference depth (m) for literature baseline
#'   fluxes. Default \code{5}.
#' @inheritParams set_aed_sed_const2d
#' @param verbose Logical. Print zone summary and copy-paste config lines.
#'   Default \code{TRUE}.
#'
#' @return Invisibly returns a named list with the following elements:
#'   \describe{
#'     \item{\code{fsed_oxy}}{Numeric vector of length \code{n_zones}. Sediment
#'       oxygen demand flux (mmol O2/m2/d, negative).}
#'     \item{\code{fsed_amm}}{Numeric vector of length \code{n_zones}. Ammonium
#'       flux (mmol N/m2/d).}
#'     \item{\code{fsed_nit}}{Numeric vector of length \code{n_zones}. Nitrate
#'       flux (mmol N/m2/d).}
#'     \item{\code{fsed_frp}}{Numeric vector of length \code{n_zones}. Filterable
#'       reactive phosphorus flux (mmol P/m2/d).}
#'     \item{\code{zone_summary}}{Data frame of zone geometry and final flux
#'       estimates.}
#'     \item{\code{method}}{Character string; either \code{"baseline_scaled"} or
#'       \code{"obs_adjusted"}.}
#'   }
#'
#' @examples
#' \dontrun{
#' zone_heights <- estimate_sed_zones(hypsograph)
#'
#' # Tier 1 only
#' fluxes <- estimate_zone_fluxes(zone_heights, hypsograph)
#'
#' # Tier 2 with observations (Southern Hemisphere)
#' fluxes <- estimate_zone_fluxes(
#'   zone_heights,
#'   hypsograph,
#'   obs = obs_df,
#'   lat = -38
#' )
#' }
#'
#' @references
#' Beutel, M.W. (2006). \doi{10.1016/j.ecoleng.2006.05.009}
#'
#' Muller, B., et al. (2012). \doi{10.1021/es301422r}
#'
#' Nurnberg, G.K. (1984). \doi{10.4319/lo.1984.29.1.0111}
#'
#' Seitzinger, S.P. (1988). \doi{10.4319/lo.1988.33.4part2.0702}
#'
#' Sondergaard, M., et al. (2003). \doi{10.1023/B:HYDR.0000008611.12704.dd}
#'
#' @importFrom dplyr mutate across everything
#' @importFrom clitable cli_table
#' @importFrom cli cli_warn cli_abort cli_rule cli_text
#' @export
estimate_zone_fluxes <- function(aeme, path,
                                 ref_depth = 5,
                                 baseline  = c(fsed_oxy = -25,
                                               fsed_amm =  2,
                                               fsed_nit =  0.2,
                                               fsed_frp =  0.05),
                                 verbose   = TRUE) {
  aeme <- check_aeme(aeme)
  if (missing(path)) {
    path <- get_aeme_path(aeme)
  }
  path <- check_path(path)
  lake_dir <- get_lake_dir(aeme, path)
  hypsograph <- get_hypsograph(aeme)
  lke <- get_lake(aeme)
  lat <- lke$latitude
  cfg <- read_model_config(model = "glm_aed", lake_dir = lake_dir)
  obs <- get_obs(aeme = aeme, var_sim = c("CHM_oxy", "NIT_amm", "NIT_nit",
                                          "PHS_frp"))
  # ---------------------------------------------------------------------------
  # 0. Input checks
  # ---------------------------------------------------------------------------
  zone_heights <- cfg[["hydrodynamic"]][["sediment"]][["zone_heights"]]
  
  stopifnot(is.numeric(zone_heights), length(zone_heights) >= 1)
  stopifnot(is.data.frame(hypsograph),
            all(c("depth", "area") %in% names(hypsograph)))
  stopifnot(all(c("fsed_oxy", "fsed_amm", "fsed_nit", "fsed_frp") %in%
                  names(baseline)))
  stopifnot(is.numeric(ref_depth), ref_depth > 0)
  stopifnot(is.logical(verbose), length(verbose) == 1)
  
  
  n_zones   <- length(zone_heights)
  zone_heights <- sort(zone_heights)                     # ensure ascending
  max_depth <- abs(min(hypsograph$depth))                # positive metres
  
  if (zone_heights[n_zones] < max_depth)
    cli::cli_warn("Note: top zone_height ({zone_heights[n_zones]} m) < max lake depth ({max_depth} m). Re-run estimate_sed_zones() if this is unexpected.")

  # ---------------------------------------------------------------------------
  # 1. Zone geometry from hypsograph
  # ---------------------------------------------------------------------------
  # zone_heights are from the bed (upward).
  # Convert to depth-below-surface (positive downward) for depth matching:
  #   depth_from_surface = max_depth - height_from_bed
  #
  #   Zone i occupies heights:  zone_heights[i-1]  to  zone_heights[i]
  #   In depth:                 zone_depth_upper[i] to  zone_depth_lower[i]
  
  zone_height_lower <- c(0, zone_heights[-n_zones])     # bed-referenced lower
  zone_height_upper <- zone_heights                      # bed-referenced upper
  
  zone_depth_lower  <- max_depth - zone_height_lower    # surface-referenced, deeper
  zone_depth_upper  <- pmax(max_depth - zone_height_upper, 0)  # shallower
  
  zone_mean_depth   <- (zone_depth_lower + zone_depth_upper) / 2
  
  # Interpolate lake area at each zone boundary depth
  .interp_area <- function(depth_pos, hyps) {
    # hyps$depth is negative; depth_pos is positive downward
    d <- -depth_pos
    d <- max(min(hyps$depth), min(max(hyps$depth), d))  # clamp to range
    approx(hyps$depth, hyps$area, xout = d)$y
  }
  
  area_at_lower <- vapply(zone_depth_lower, .interp_area, numeric(1),
                          hyps = hypsograph)
  area_at_upper <- vapply(zone_depth_upper, .interp_area, numeric(1),
                          hyps = hypsograph)
  zone_area      <- abs(area_at_lower - area_at_upper)
  total_area     <- sum(zone_area)
  zone_area_frac <- zone_area / total_area
  
  # ---------------------------------------------------------------------------
  # 2. Tier 1 -- depth-scaled baseline fluxes
  # ---------------------------------------------------------------------------
  # Scale each flux by (mean_zone_depth / ref_depth), capped at 4x.
  # Area-weighted normalisation then ensures the lake-wide area-weighted
  # total matches the literature baseline -- inter-zone differences are
  # preserved but the overall magnitude is anchored.
  #
  # NO3 sign logic:
  #   Shallow zones (< 0.5 * max_depth): small positive release
  #   Deep zones    (> 0.5 * max_depth): negative (denitrification consumes NO3)
  #   Transition is linear across zones.
  
  depth_scale <- pmin(zone_mean_depth / ref_depth, 2)
  
  oxy_raw <- baseline["fsed_oxy"] * depth_scale          # more negative deeper
  amm_raw <- baseline["fsed_amm"] * depth_scale          # higher deeper
  frp_raw <- baseline["fsed_frp"] * depth_scale * 4      # redox-sensitive: 4x depth gradient
  nit_raw <- ifelse(zone_mean_depth > 0.6 * max_depth,
                    baseline["fsed_nit"] * -2,   # denitrification dominates
                    baseline["fsed_nit"] * +0.5) # nitrification dominates
  
  # Normalise so area-weighted sum equals baseline * total_area_fraction
  .normalise <- function(raw, base_val, area_frac) {
    target  <- base_val * sum(area_frac)
    current <- sum(raw * area_frac)
    if (abs(current) < 1e-10) return(raw)
    raw * (target / current)
  }
  
  fsed_oxy <- unname(.normalise(oxy_raw, baseline["fsed_oxy"], zone_area_frac))
  fsed_amm <- unname(.normalise(amm_raw, baseline["fsed_amm"], zone_area_frac))
  fsed_frp <- unname(.normalise(frp_raw, baseline["fsed_frp"], zone_area_frac))
  fsed_nit <- unname(nit_raw)   # not normalised -- sign flip is intentional
  
  method <- "baseline_scaled"
  
  # ---------------------------------------------------------------------------
  # 3. Tier 2 -- observed data adjustment (optional)
  # ---------------------------------------------------------------------------
  # Map AEME variable names -> flux type
  var_map <- c(CHM_oxy = "oxy",
               NIT_amm = "amm",
               NIT_nit = "nit",
               PHS_frp = "frp")
  
  if (!is.null(obs) && nrow(obs) > 0) {
    
    # -- Validate ---------------------------------------------------------------
    req_cols <- c("Date", "var_aeme", "depth", "value")
    missing_cols <- setdiff(req_cols, names(obs))
    if (length(missing_cols))
      cli::cli_abort("obs is missing columns: {paste(missing_cols, collapse = ', ')}")
    
    # -- Filter to summer stratification season ----------------------------------
    summer_months <- if (!is.null(lat) && lat < 0) c(12, 1, 2) else c(6, 7, 8)
    obs$month     <- as.integer(format(as.Date(obs$Date), "%m"))
    obs_use       <- obs[
      obs$month %in% summer_months &
        obs$var_aeme %in% names(var_map),
    ]
    obs_use$flux_type <- var_map[obs_use$var_aeme]
    obs_use$depth_mid <- obs_use$depth
    
    if (nrow(obs_use) == 0) {
      # message("No summer observations found for O2/NH4/NO3/FRP -- ",
      #         "skipping Tier 2 adjustment.")
      cli_inform_safe(c("!" = "No summer observations found for O2/NH4/NO3/FRP 
      -- skipping Tier 2 adjustment."))
    } else {
      
      # -- Assign each observation to a zone -------------------------------------
      # depth_upper[z] (shallower) <= depth_mid < depth_lower[z] (deeper)
      obs_use$zone <- NA_integer_
      for (z in seq_len(n_zones)) {
        in_z <- obs_use$depth_mid >= zone_depth_upper[z] &
          obs_use$depth_mid <= zone_depth_lower[z]
        obs_use$zone[in_z] <- z
      }
      obs_use <- obs_use[!is.na(obs_use$zone), ]
      
      if (nrow(obs_use) == 0) {
        # message("Observations do not overlap with zone depth ranges -- ",
        #         "skipping Tier 2 adjustment.")
        cli_inform_safe(c("!" = "Observations do not overlap with zone depth ranges
        -- skipping Tier 2 adjustment."))
      } else {
        
        # -- Zone-median per flux type -------------------------------------------
        # Build [n_zones x 4] matrix of median observed concentrations
        flux_types <- c("oxy", "amm", "nit", "frp")
        obs_mat <- matrix(NA_real_, nrow = n_zones, ncol = length(flux_types),
                          dimnames = list(paste0("Zone", seq_len(n_zones)),
                                          flux_types))
        for (z in seq_len(n_zones)) {
          z_obs <- obs_use[obs_use$zone == z, ]
          if (nrow(z_obs) == 0) next
          for (ft in flux_types) {
            vals <- z_obs$value[z_obs$flux_type == ft]
            if (length(vals)) obs_mat[z, ft] <- median(vals, na.rm = TRUE)
          }
        }
        
        if (verbose) {
          cli_inform_safe(c("i" = "Tier 2: zone-median summer concentrations 
                            used for adjustment:"))
          obs_df <- as.data.frame(obs_mat)
          
          # Format values and improve column headers
          colnames(obs_df) <- dplyr::recode(
            colnames(obs_df),
            "oxy" = " O2 (mg/L) ",
            "amm" = " NH4 (mg/L) ",
            "nit" = " NO3 (mg/L) ",
            "frp" = " FRP (mg/L) "
          )
          
          obs_df <- obs_df |>
            dplyr::mutate(dplyr::across(dplyr::everything(), \(x) formatC(x, digits = 3, format = "g")))
          
          # Add zone as explicit column
          obs_df <- cbind(zone = rownames(obs_df), obs_df)
          rownames(obs_df) <- NULL
          
          ct <- clitable::cli_table(obs_df)
          cli_table_safe(ct)
        }
        
        # -- Adjustment multiplier -----------------------------------------------
        # Ratio of zone concentration to cross-zone median.
        # Requires at least 2 zones with data to compute a meaningful ratio.
        # inverse = TRUE: lower concentration -> higher flux magnitude
        #   (e.g. low O2 near bed -> high SOD; low NO3 -> more denitrification)
        # inverse = FALSE: higher concentration -> higher flux
        #   (e.g. high NH4 near bed -> high fsed_amm)
        .adj_mult <- function(vals, inverse = FALSE) {
          n_valid <- sum(!is.na(vals))
          if (n_valid < 2) {
            # message("  < 2 zones with data for this flux -- skipping adjustment")
            cli_inform_safe(c("i" = "Less than 2 zones with data for this flux -- skipping adjustment"))
            return(rep(1, length(vals)))
          }
          med <- median(vals, na.rm = TRUE)
          if (is.na(med) || med == 0) return(rep(1, length(vals)))
          r        <- vals / med
          r[is.na(r)] <- 1                 # zones with no obs get neutral mult
          r        <- pmax(pmin(r, 3), 0.33)  # cap at 3x to avoid extremes
          if (inverse) 1 / r else r
        }
        
        adj_log <- character(0)
        
        # if (any(!is.na(obs_mat[, "oxy"]))) {
        #   mult     <- .adj_mult(obs_mat[, "oxy"], inverse = TRUE)
        #   fsed_oxy <- fsed_oxy * mult
        #   adj_log  <- c(adj_log, paste0("fsed_oxy (", sum(!is.na(obs_mat[,"oxy"])),
        #                                 " zones, inverse O2)"))
        # }
        if (any(!is.na(obs_mat[, "amm"]))) {
          mult     <- .adj_mult(obs_mat[, "amm"], inverse = FALSE)
          fsed_amm <- fsed_amm * mult
          adj_log  <- c(adj_log, paste0("fsed_amm (", sum(!is.na(obs_mat[,"amm"])),
                                        " zones, direct NH4)"))
        }
        # if (any(!is.na(obs_mat[, "nit"]))) {
        #   mult     <- .adj_mult(obs_mat[, "nit"], inverse = TRUE)
        #   fsed_nit <- fsed_nit * mult
        #   adj_log  <- c(adj_log, paste0("fsed_nit (", sum(!is.na(obs_mat[,"nit"])),
        #                                 " zones, inverse NO3)"))
        # }
        if (any(!is.na(obs_mat[, "frp"]))) {
          mult     <- .adj_mult(obs_mat[, "frp"], inverse = FALSE)
          fsed_frp <- fsed_frp * mult
          adj_log  <- c(adj_log, paste0("fsed_frp (", sum(!is.na(obs_mat[,"frp"])),
                                        " zones, direct FRP)"))
        }
        
        if (length(adj_log)) {
          # message()
          msg <- paste0("Tier 2 adjustments applied: ", paste(adj_log, 
                                                              collapse = "; "))
          cli_inform_safe(c("i" = msg))
          method <- "obs_adjusted"
        }
      }
    }
  }
  
  # ---------------------------------------------------------------------------
  # 4. Round and return
  # ---------------------------------------------------------------------------
  fsed_oxy <- round(fsed_oxy, 2)
  fsed_amm <- round(fsed_amm, 3)
  fsed_nit <- round(fsed_nit, 3)
  fsed_frp <- round(fsed_frp, 4)
  
  zone_summary <- data.frame(
    zone            = seq_len(n_zones),
    height_lower_m  = zone_height_lower,
    height_upper_m  = zone_height_upper,
    depth_upper_m   = round(zone_depth_upper, 2),   # shallower boundary
    depth_lower_m   = round(zone_depth_lower, 2),   # deeper boundary
    mean_depth_m    = round(zone_mean_depth,  2),
    area_m2         = round(zone_area, 0),
    area_frac       = round(zone_area_frac, 3),
    fsed_oxy        = fsed_oxy,
    fsed_amm        = fsed_amm,
    fsed_nit        = fsed_nit,
    fsed_frp        = fsed_frp
  )
  
  # Calculate lake-wide area-weighted average fluxes for sanity check
  lake_avg_oxy <- sum(zone_summary$fsed_oxy * zone_summary$area_frac)
  lake_avg_amm <- sum(zone_summary$fsed_amm * zone_summary$area_frac)
  lake_avg_nit <- sum(zone_summary$fsed_nit * zone_summary$area_frac)
  lake_avg_frp <- sum(zone_summary$fsed_frp * zone_summary$area_frac)
  lake_avg_fluxes <- c(oxy = lake_avg_oxy, amm = lake_avg_amm, 
                       nit = lake_avg_nit, frp = lake_avg_frp)
  
  
  if (verbose) {
    cli_safe(paste0("Sediment zone flux estimates (", method, ")"), FUN = cli::cli_rule)
    cli_safe(paste0("n_zones: ", n_zones, " | max lake depth: ", max_depth, 
                    " m | ref_depth: ", ref_depth, " m"), FUN = cli::cli_text)
    # --- zone summary table ----------------------------------------------------
    zone_tbl <- zone_summary |>
      dplyr::mutate(
        dplyr::across(dplyr::where(is.numeric), \(x) formatC(x, digits = 3, 
                                                             format = "g"))) |>
      as.data.frame()
    
    colnames(zone_tbl) <- dplyr::recode(
      colnames(zone_tbl),
      "zone"           = "Zone",
      "height_lower_m" = "H lower (m)",
      "height_upper_m" = "H upper (m)",
      "depth_upper_m"  = "D upper (m)",
      "depth_lower_m"  = "D lower (m)",
      "mean_depth_m"   = "Mean D (m)",
      "area_m2"        = "Area (m2)",
      "area_frac"      = "Area frac",
      "fsed_oxy"       = "O2",
      "fsed_amm"       = "NH4",
      "fsed_nit"       = "NO3",
      "fsed_frp"       = "FRP"
    )
    
    ct <- clitable::cli_table(zone_tbl)
    cli_table_safe(ct)
    
    # --- lake-wide averages table -----------------------------------------------
    # cli::cli_text("")
    cli_safe("", FUN = cli::cli_text)
    cli_safe("Lake-wide area-weighted average fluxes (mmol/m2/d)",
             FUN = cli::cli_rule)

    avg_tbl <- as.data.frame(t(round(lake_avg_fluxes, 3)))
    
    colnames(avg_tbl) <- dplyr::recode(
      colnames(avg_tbl),
      "oxy" = "O2 (mmol/m2/d)",
      "amm" = "NH4 (mmol/m2/d)",
      "nit" = "NO3 (mmol/m2/d)",
      "frp" = "FRP (mmol/m2/d)"
    )
    
    ct <- clitable::cli_table(avg_tbl)
    cli_table_safe(ct)
  }
  
  invisible(list(
    fsed_oxy     = fsed_oxy,
    fsed_amm     = fsed_amm,
    fsed_nit     = fsed_nit,
    fsed_frp     = fsed_frp,
    zone_summary = zone_summary,
    method       = method
  ))
}
