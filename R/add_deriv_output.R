#' Add derived output variables to model output
#'
#' @param out_list list of model output variables
#' @param hyps data frame with hypsograph data
#'
#' @returns List of model output variables with derived variables added
#' @export
#'
add_deriv_output <- function(out_list, hyps) {
  
  hyps <- prepare_hyps(hyps)
  
  .deriv_registry <- list(
    lakeanalyzer = calc_lakeanalyzer,
    schstb       = calc_schstb,
    oxygen       = calc_oxygen,
    tli          = calc_tli_module
  )
  
  # Default: run all modules
  # if (is.null(vars_sim)) {
    vars_sim <- names(.deriv_registry)
  # }
  
  # vars_sim <- intersect(vars_sim, names(.deriv_registry))
  
  for (v in vars_sim) {
    newvals <- .deriv_registry[[v]](out_list, hyps)
    if (!is.null(newvals)) {
      out_list[names(newvals)] <- newvals
    }
  }
  
  out_list
}

#' TLI calculation functions
#' @noRd
calc_tli_module <- function(out_list, hyps) {
  needed <- c("PHS_tp", "NIT_tn", "PHY_tchla", "HYD_epidep", "LKE_photic")
  if (!all(needed %in% names(out_list))) return(NULL)
  
  depths <- out_list[["LKE_depths"]]
  epidep <- out_list[["HYD_epidep"]]
  
  res <- lapply(seq_len(ncol(depths)), function(c) {
    idx <- get_epi(depths[, c], epidep[c])
    
    chla   <- mean(out_list$PHY_tchla[idx, c], na.rm = TRUE)
    tn     <- mean(out_list$NIT_tn[idx, c], na.rm = TRUE)
    tp     <- mean(out_list$PHS_tp[idx, c], na.rm = TRUE)
    secchi <- out_list$LKE_photic[c]
    
    data.frame(
      tli_c     = calc_tli_chla(chla),
      tli_n     = calc_tli_n(tn),
      tli_p     = calc_tli_p(tp),
      tli_secchi = calc_tli_secchi(secchi),
      tli_3     = calc_tli3(chla, tn, tp),
      tli_4     = calc_tli4(chla, tn, tp, secchi)
    )
  }) |> dplyr::bind_rows()
  
  list(
    LKE_tlic = res$tli_c,
    LKE_tlin = res$tli_n,
    LKE_tlip = res$tli_p,
    LKE_tlise = res$tli_secchi,
    LKE_tli3 = res$tli_3,
    LKE_tli4 = res$tli_4
  )
}

#' Oxygen calculation functions
#' @noRd
calc_oxygen <- function(out_list, hyps) {
  if (!"CHM_oxy" %in% names(out_list))
    return(NULL)
  
  oxy     <- out_list[["CHM_oxy"]]
  if (is.null(oxy)) return(NULL)
  wtr     <- out_list[["HYD_temp"]]
  depths  <- out_list[["LKE_depths"]]
  lake_level <- out_list[["LKE_lvlwtr"]]
  
  epidep <- out_list$HYD_epidep
  hypdep <- out_list$HYD_hypdep
  
  # Oxycline
  oxycline <- safe_apply(ncol(oxy), function(c) {
    v <- cline_depth(oxy[, c], depths[, c], water = FALSE)
    if (is.nan(v)) NA_real_ else v
  })
  
  epi_oxy <- safe_apply(ncol(wtr), function(c) {
    idx <- get_epi(depths[, c], epidep[c])
    mean(oxy[idx, c], na.rm = TRUE)
  })
  
  hyp_oxy <- safe_apply(ncol(wtr), function(c) {
    idx <- get_hyp(depths[, c], hypdep[c])
    mean(oxy[idx, c], na.rm = TRUE)
  })
  
  meta_oxy <- safe_apply(ncol(wtr), function(c) {
    idx <- get_meta(depths[, c], epidep[c], hypdep[c])
    mean(oxy[idx, c], na.rm = TRUE)
  })
  
  exp_oxy <- (epi_oxy + hyp_oxy) / 2
  
  # Number anoxic layers
  anox_layers <- safe_apply(ncol(wtr), function(c) {
    if (all(is.na(oxy[, c])) || length(unique(depths[, c])) <= 1)
      return(NA_real_)
    
    oxy_layers <- approx(
      y = oxy[, c], x = depths[, c],
      xout = seq(0, lake_level[c], by = 0.2),
      rule = 2
    )$y
    
    sum(oxy_layers < 1)
  })
  
  list(
    CHM_oxycln = oxycline,
    CHM_oxyepi = epi_oxy,
    CHM_oxyhyp = hyp_oxy,
    CHM_oxymet = meta_oxy,
    CHM_oxymom = meta_oxy - exp_oxy,
    CHM_oxynal = anox_layers
  )
}

#' Schmidt stability calculation function
#' @noRd
calc_schstb <- function(out_list, hyps) {
  wtr     <- out_list[["HYD_temp"]]
  depths  <- out_list[["LKE_depths"]]
  
  res <- safe_apply(ncol(wtr), function(c) {
    
    bthD <- c(0, depths[, c])
    bthA <- approx(x = hyps$full_depth, y = hyps$area,
                   xout = bthD, rule = 2)$y
    
    if (any(is.na(bthA)) || length(unique(bthA)) <= 1 ||
        sum(!is.na(wtr[, c])) <= 1) return(NA_real_)
    
    v <- rLakeAnalyzer::schmidt.stability(
      wtr    = wtr[, c],
      depths = depths[, c],
      bthA   = bthA,
      bthD   = bthD
    )
    if (is.nan(v)) NA_real_ else v
  })
  
  list(HYD_schstb = res)
}

#' LakeAnalyzer derived variable calculations
#' @noRd
calc_lakeanalyzer <- function(out_list, hyps) {
  wtr     <- out_list[["HYD_temp"]]
  depths  <- out_list[["LKE_depths"]]
  
  fun_list <- list(
    HYD_thmcln = rLakeAnalyzer::thermo.depth,
    HYD_strat  = is_strat,
    HYD_ctrbuy = rLakeAnalyzer::center.buoyancy,
    HYD_epidep = rLakeAnalyzer::meta.depths,
    HYD_hypdep = rLakeAnalyzer::meta.depths
  )
  
  res <- lapply(names(fun_list), function(f) {
    idx <- if (f == "HYD_hypdep") 2 else 1
    
    safe_apply(ncol(wtr), function(c) {
      if (all(is.na(wtr[, c]))) return(NA_real_)
      v <- fun_list[[f]](wtr[, c], depths[, c])
      v[is.nan(v)] <- NA_real_
      v[idx]
    })
  })
  names(res) <- names(fun_list)
  res
}

#' Prepare hypsograph by adding full_depth column
#' @noRd
prepare_hyps <- function(hyps) {
  hyps$full_depth <- max(hyps$elev) - hyps$elev
  hyps
}


# Return NA if all-values-NA
safe_apply <- function(n, f) {
  vapply(seq_len(n), f, numeric(1))
}

# Get epi/hyp/meta indices
get_epi <- function(depths, e)  which(depths <= e)
get_hyp <- function(depths, h)  which(depths >= h)
get_meta <- function(depths, e, h) which(depths >= e & depths < h)


# add_deriv_output <- function(out_list, hyps) {
#   
#   # Determine z_step
#   # depth <- out_list[["depth"]]
#   # z_step <- if (mean(depth, na.rm = TRUE) < 10) 0.2 else 0.5
#   
#   # Input objects
#   # inp <- input(aeme)
#   # hyps <- inp$hypsograph
#   hyps$full_depth <- max(hyps$elev) - hyps$elev
#   
#   # Temperature & depths
#   wtr <- out_list[["HYD_temp"]]
#   depths <- out_list[["LKE_depths"]]
#   lake_level <- out_list[["LKE_lvlwtr"]]
#   
#   # LakeAnalyzer functions
#   fun_list <- list(
#     HYD_thmcln = rLakeAnalyzer::thermo.depth,
#     HYD_strat  = is_strat,
#     HYD_ctrbuy = rLakeAnalyzer::center.buoyancy,
#     HYD_epidep = rLakeAnalyzer::meta.depths,
#     HYD_hypdep = rLakeAnalyzer::meta.depths
#   )
#   
#   laz_list <- lapply(names(fun_list), \(f) {
#     idx <- ifelse(f == "HYD_hypdep", 2, 1)
#     vapply(1:ncol(wtr), \(c) {
#       if (all(is.na(wtr[, c]))) return(NA)
#       v <- fun_list[[f]](wtr = wtr[, c], depths = depths[, c])
#       v[is.nan(v)] <- NA
#       v[idx]
#     }, numeric(1))
#   })
#   names(laz_list) <- names(fun_list)
#   
#   # Schmidt stability
#   laz_list[["HYD_schstb"]] <- vapply(1:ncol(wtr), \(c) {
#     bthD <- c(0, depths[, c])
#     bthA <- approx(x = hyps$full_depth, y = hyps$area,
#                    xout = bthD, rule = 2)$y
#     
#     if (any(is.na(bthA)) || length(unique(bthA)) <= 1 ||
#         sum(!is.na(wtr[, c])) <= 1) return(NA)
#     
#     v <- rLakeAnalyzer::schmidt.stability(
#       wtr = wtr[, c],
#       depths = depths[, c],
#       bthA = bthA,
#       bthD = bthD
#     )
#     v[is.nan(v)] <- NA
#     v
#   }, numeric(1))
#   
#   # Add to out_list
#   for (n in names(laz_list)) out_list[[n]] <- laz_list[[n]]
#   
#   # Oxygen calculations
#   if ("CHM_oxy" %in% names(out_list) & !is.null(out_list[["CHM_oxy"]])) {
#     oxy <- out_list[["CHM_oxy"]]
#     
#     # Oxycline
#     oxy_cline <- vapply(1:ncol(oxy), \(c) {
#       v <- cline_depth(wtr = oxy[, c], depths = depths[, c], water = FALSE)
#       v[is.nan(v)] <- NA
#       v
#     }, numeric(1))
#     
#     # Epilimnion / Hypolimnion / Metalimnion oxygen
#     epi_oxy <- vapply(1:ncol(wtr), \(c) {
#       idx <- which(depths[, c] <= laz_list$HYD_epidep[c])
#       mean(oxy[idx, c])
#     }, numeric(1))
#     
#     hyp_oxy <- vapply(1:ncol(wtr), \(c) {
#       idx <- which(depths[, c] >= laz_list$HYD_hypdep[c])
#       mean(oxy[idx, c])
#     }, numeric(1))
#     
#     meta_oxy <- vapply(1:ncol(wtr), \(c) {
#       idx <- which(depths[, c] >= laz_list$HYD_epidep[c] &
#                      depths[, c] < laz_list$HYD_hypdep[c])
#       mean(oxy[idx, c])
#     }, numeric(1))
#     
#     exp_oxy <- (epi_oxy + hyp_oxy) / 2
#     
#     # Add oxygen outputs
#     out_list$CHM_oxycln <- oxy_cline
#     out_list$CHM_oxyepi <- epi_oxy
#     out_list$CHM_oxyhyp <- hyp_oxy
#     out_list$CHM_oxymet <- meta_oxy
#     out_list$CHM_oxymom <- meta_oxy - exp_oxy
#     
#     # Number of anoxic layers
#     out_list$CHM_oxynal <- vapply(1:ncol(wtr), \(c) {
#       if (all(is.na(oxy[, c])) || length(unique(depths[, c])) <= 1)
#         return(NA)
#       
#       oxy_layers <- approx(
#         y = oxy[, c], x = depths[, c],
#         xout = seq(0, lake_level[c], by = 0.2),
#         rule = 2
#       )$y
#       
#       sum(oxy_layers < 1)
#     }, numeric(1))
#   }
#   
#   # Switch for calculating TLI
#   calc_tli <- all(c("PHS_tp", "NIT_tn", "PHY_tchla", "HYD_epidep") %in%
#                     names(out_list))
#   # Calculate TLI
#   if (calc_tli) {
#     tlc <- lapply(1:ncol(wtr), \(c) {
#       idx <- which(depths[, c] <= laz_list$HYD_epidep[c])
#       if (is.na(laz_list$HYD_epidep[c])) {
#         idx <- nrow(depths)
#       }
#       chla <- mean(out_list[["PHY_tchla"]][idx, c])
#       tn <- mean(out_list[["NIT_tn"]][idx, c]) #* 1000
#       tp <- mean(out_list[["PHS_tp"]][idx, c]) #* 1000
#       secchi <- out_list[["LKE_photic"]][c]
#       
#       tli_c <- calc_tli_chla(chla)
#       tli_n <-  calc_tli_n(tn)
#       tli_p <-  calc_tli_p(tp)
#       tli_secchi <-  calc_tli_secchi(secchi)
#       
#       tli_3 <- calc_tli3(chla = chla, tn = tn, tp = tp)
#       tli_4 <- calc_tli4(chla = chla, tn = tn, tp = tp, secchi = secchi)
#       
#       data.frame(tli_c = tli_c, tli_n = tli_n, tli_p = tli_p,
#                  tli_secchi = tli_secchi, tli_3 = tli_3, tli_4 = tli_4)
#       
#     }) |>
#       dplyr::bind_rows()
#     
#     out_list[["LKE_tlic"]] <- as.vector(tlc$tli_c)
#     out_list[["LKE_tlin"]] <- as.vector(tlc$tli_n)
#     out_list[["LKE_tlip"]] <- as.vector(tlc$tli_p)
#     out_list[["LKE_tlise"]] <- as.vector(tlc$tli_secchi)
#     
#     out_list[["LKE_tli3"]] <- as.vector(tlc$tli_3)
#     out_list[["LKE_tli4"]] <- as.vector(tlc$tli_4)
#     
#   }
#   
#   return(out_list)
# }
