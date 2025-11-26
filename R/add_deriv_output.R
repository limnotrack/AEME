#' Add derived output variables to model output
#'
#' @param out_list list of model output variables
#' @param hyps data frame with hypsograph data
#'
#' @returns List of model output variables with derived variables added
#' @export

add_deriv_output <- function(out_list, hyps, vars_sim = NULL) {
  
  hyps <- prepare_hyps(hyps)
  
  if (is.null(vars_sim)) {
    vars_sim <- names(.deriv_deps)
  }
  # Step 1: expand variable set with dependencies
  all_vars <- resolve_dependencies(vars_sim = vars_sim)
  
  # Step 2: run in dependency order
  orig_vars <- names(out_list)
  for (v in all_vars) {
    fun <- .deriv_registry[[v]]
    out_list[[v]] <- fun(out_list, hyps)
  }
  
  # Step 3: select only requested variables
  out_list <- out_list[intersect(names(out_list), c(orig_vars, vars_sim))]
  
  out_list
}


#' Thermocline depth calculation function
#' @noRd
calc_HYD_thmcln <- function(out_list, hyps) {
  req_vars <- c("HYD_temp", "LKE_depths")
  var_check <- check_vars(out_list, req_vars)
  if (!var_check) {
    return(NULL)
  }
  max_dep <- abs(min(hyps$depth))
  wtr    <- out_list[["HYD_temp"]]
  depths <- out_list[["LKE_depths"]]
  
  safe_apply(ncol(wtr), function(c) {
    if (all(is.na(wtr[, c]))) return(NA_real_)
    v <- rLakeAnalyzer::thermo.depth(wtr[, c], depths[, c])
    if (is.nan(v)) max_dep else v
  })
}

#' Stratification status calculation function
#' @noRd
calc_HYD_strat <- function(out_list, hyps) {
  req_vars <- c("HYD_temp", "LKE_depths")
  var_check <- check_vars(out_list, req_vars)
  if (!var_check) {
    return(NULL)
  }
  wtr    <- out_list[["HYD_temp"]]
  depths <- out_list[["LKE_depths"]]
  
  safe_apply(ncol(wtr), function(c) {
    if (all(is.na(wtr[, c]))) return(NA_real_)
    v <- is_strat(wtr[, c], depths[, c])
    if (is.nan(v)) NA_real_ else v
  })
}

#' Schmidt stability calculation function
#' @noRd
calc_HYD_schstb <- function(out_list, hyps) {
  req_vars <- c("HYD_temp", "LKE_depths")
  var_check <- check_vars(out_list, req_vars)
  if (!var_check) {
    return(NULL)
  }
  wtr    <- out_list[["HYD_temp"]]
  depths <- out_list[["LKE_depths"]]
  
  safe_apply(ncol(wtr), function(c) {
    
    if (all(is.na(wtr[, c]))) return(NA_real_)
    bthD <- c(0, depths[, c])
    bthA <- approx(x = hyps$full_depth, y = hyps$area,
                   xout = bthD, rule = 2)$y
    
    if (any(is.na(bthA))) return(NA_real_)
    
    v <- rLakeAnalyzer::schmidt.stability(
      wtr    = wtr[, c],
      depths = depths[, c],
      bthA   = bthA,
      bthD   = bthD
    )
    
    if (is.nan(v)) NA_real_ else v
  })
}

#' Center of buoyancy calculation function
#' @noRd
calc_HYD_ctrbuy <- function(out_list, hyps) {
  req_vars <- c("HYD_temp", "LKE_depths")
  var_check <- check_vars(out_list, req_vars)
  if (!var_check) {
    return(NULL)
  }
  wtr    <- out_list[["HYD_temp"]]
  depths <- out_list[["LKE_depths"]]
  
  safe_apply(ncol(wtr), function(c) {
    if (all(is.na(wtr[, c]))) return(NA_real_)
    v <- rLakeAnalyzer::center.buoyancy(wtr[, c], depths[, c])
    if (is.nan(v)) NA_real_ else v
  })
}

#' Epilimnion depth calculation function
#' @noRd
calc_HYD_epidep <- function(out_list, hyps) {
  req_vars <- c("HYD_temp", "LKE_depths")
  var_check <- check_vars(out_list, req_vars)
  if (!var_check) {
    return(NULL)
  }
  wtr    <- out_list[["HYD_temp"]]
  depths <- out_list[["LKE_depths"]]
  
  safe_apply(ncol(wtr), function(c) {
    if (all(is.na(wtr[, c]))) return(NA_real_)
    v <- rLakeAnalyzer::meta.depths(wtr[, c], depths[, c])
    if (is.nan(v[1])) max(depths[, c]) else v[1]
  })
}

#' Hypolimnion depth calculation function
#' @noRd
calc_HYD_hypdep <- function(out_list, hyps) {
  req_vars <- c("HYD_temp", "LKE_depths")
  var_check <- check_vars(out_list, req_vars)
  if (!var_check) {
    return(NULL)
  }
  wtr    <- out_list[["HYD_temp"]]
  depths <- out_list[["LKE_depths"]]
  
  safe_apply(ncol(wtr), function(c) {
    if (all(is.na(wtr[, c]))) return(NA_real_)
    v <- rLakeAnalyzer::meta.depths(wtr[, c], depths[, c])
    if (is.nan(v[2])) NA_real_ else v[2]
  })
}

#' Oxycline depth calculation function
#' @noRd
calc_CHM_oxycln <- function(out_list, hyps) {
  req_vars <- c("CHM_oxy", "LKE_depths")
  var_check <- check_vars(out_list, req_vars)
  if (!var_check) {
    return(NULL)
  }
  oxy    <- out_list[["CHM_oxy"]]
  depths <- out_list[["LKE_depths"]]
  
  safe_apply(ncol(oxy), function(c) {
    if (all(is.na(oxy[, c]))) return(NA_real_)
    v <- cline_depth(oxy[, c], depths[, c], water = FALSE)
    if (is.nan(v)) NA_real_ else v
  })
}

#' Epilimnion oxygen calculation function
#' @noRd
calc_CHM_oxyepi <- function(out_list, hyps) {
  req_vars <- c("CHM_oxy", "LKE_depths", "HYD_epidep")
  var_check <- check_vars(out_list, req_vars)
  if (!var_check) {
    return(NULL)
  }
  oxy    <- out_list[["CHM_oxy"]]
  depths <- out_list[["LKE_depths"]]
  epi    <- out_list[["HYD_epidep"]]
  
  safe_apply(ncol(oxy), function(c) {
    if (all(is.na(oxy[, c]))) return(NA_real_)
    idx <- which(depths[, c] <= epi[c])
    mean(oxy[idx, c], na.rm = TRUE)
  })
}

#' Hypolimnion oxygen calculation function
#' @noRd
calc_CHM_oxyhyp <- function(out_list, hyps) {
  req_vars <- c("CHM_oxy", "LKE_depths", "HYD_hypdep")
  var_check <- check_vars(out_list, req_vars)
  if (!var_check) {
    return(NULL)
  }
  oxy    <- out_list[["CHM_oxy"]]
  depths <- out_list[["LKE_depths"]]
  hyp    <- out_list[["HYD_hypdep"]]
  
  safe_apply(ncol(oxy), function(c) {
    if (all(is.na(oxy[, c]))) return(NA_real_)
    idx <- which(depths[, c] >= hyp[c])
    mean(oxy[idx, c], na.rm = TRUE)
  })
}

#' Metalimnion oxygen calculation function
#' @noRd
calc_CHM_oxymet <- function(out_list, hyps) {
  req_vars <- c("CHM_oxy", "LKE_depths", "HYD_epidep", "HYD_hypdep")
  var_check <- check_vars(out_list, req_vars)
  if (!var_check) {
    return(NULL)
  }
  oxy    <- out_list[["CHM_oxy"]]
  depths <- out_list[["LKE_depths"]]
  epi    <- out_list[["HYD_epidep"]]
  hyp    <- out_list[["HYD_hypdep"]]
  
  safe_apply(ncol(oxy), function(c) {
    if (all(is.na(oxy[, c]))) return(NA_real_)
    idx <- which(depths[, c] >= epi[c] & depths[, c] < hyp[c])
    mean(oxy[idx, c], na.rm = TRUE)
  })
}

#' Metalimnion oxygen maximum calculation function
#' @noRd
calc_CHM_oxymom <- function(out_list, hyps) {
  req_vars <- c("CHM_oxy", "LKE_depths", "HYD_epidep", "HYD_hypdep")
  var_check <- check_vars(out_list, req_vars)
  if (!var_check) {
    return(NULL)
  }
  meta <- calc_CHM_oxymet(out_list, hyps)
  epi  <- calc_CHM_oxyepi(out_list, hyps)
  hyp  <- calc_CHM_oxyhyp(out_list, hyps)
  
  meta - (epi + hyp) / 2
}

#' Number of anoxic layers calculation function
#' @noRd
calc_CHM_oxynal <- function(out_list, hyps) {
  req_vars <- c("CHM_oxy", "LKE_depths", "LKE_lvlwtr")
  var_check <- check_vars(out_list, req_vars)
  if (!var_check) {
    return(NULL)
  }
  oxy        <- out_list[["CHM_oxy"]]
  depths     <- out_list[["LKE_depths"]]
  lake_level <- out_list[["LKE_lvlwtr"]]
  
  safe_apply(ncol(oxy), function(c) {
    if (all(is.na(oxy[, c]))) return(NA_real_)
    
    interpolated <- approx(
      y = oxy[, c], x = depths[, c],
      xout = seq(0, lake_level[c], by = 0.2),
      rule = 2
    )$y
    
    sum(interpolated < 1)
  })
}

#' @noRd
check_vars <- function(out_list, req_vars) {
  # Check if vars are NULL
  if (any(sapply(req_vars, function(v) is.null(out_list[[v]])))) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}


#' TLI chlorophyll-a calculation function
#' @noRd
calc_LKE_tlic <- function(out_list, hyps) {
  req_vars <- c("PHY_tchla", "LKE_depths", "HYD_epidep")
  var_check <- check_vars(out_list, req_vars)
  if (!var_check) {
    return(NULL)
  }
    
  depths <- out_list$LKE_depths
  epi    <- out_list$HYD_epidep
  chla   <- out_list$PHY_tchla
  
  safe_apply(ncol(depths), function(c) {
    idx <- which(depths[, c] <= epi[c])
    calc_tli_chla(mean(chla[idx, c], na.rm = TRUE))
  })
}

#' TLI total nitrogen calculation function
#' @noRd
calc_LKE_tlin <- function(out_list, hyps) {
  req_vars <- c("NIT_tn", "LKE_depths", "HYD_epidep")
  var_check <- check_vars(out_list, req_vars)
  if (!var_check) {
    return(NULL)
  }
  depths <- out_list$LKE_depths
  epi    <- out_list$HYD_epidep
  tn     <- out_list$NIT_tn
  
  safe_apply(ncol(depths), function(c) {
    idx <- which(depths[, c] <= epi[c])
    calc_tli_n(mean(tn[idx, c], na.rm = TRUE))
  })
}

#' TLI total phosphorus calculation function
#' @noRd
calc_LKE_tlip <- function(out_list, hyps) {
  req_vars <- c("PHS_tp", "LKE_depths", "HYD_epidep")
  var_check <- check_vars(out_list, req_vars)
  if (!var_check) {
    return(NULL)
  }
  depths <- out_list$LKE_depths
  epi    <- out_list$HYD_epidep
  tp     <- out_list$PHS_tp
  
  safe_apply(ncol(depths), function(c) {
    idx <- which(depths[, c] <= epi[c])
    calc_tli_p(mean(tp[idx, c], na.rm = TRUE))
  })
}

#' TLI Secchi depth calculation function
#' @noRd
calc_LKE_tlise <- function(out_list, hyps) {
  secchi <- out_list$LKE_photic
  
  safe_apply(length(secchi), function(c) {
    calc_tli_secchi(secchi[c])
  })
}

#' TLI 3 calculation function
#' @noRd
calc_LKE_tli3 <- function(out_list, hyps) {
  req_vars <- c("PHY_tchla", "NIT_tn", "PHS_tp", "LKE_depths", "HYD_epidep")
  var_check <- check_vars(out_list, req_vars)
  if (!var_check) {
    return(NULL)
  }
  depths <- out_list$LKE_depths
  epi    <- out_list$HYD_epidep
  chla   <- out_list$PHY_tchla
  tn     <- out_list$NIT_tn
  tp     <- out_list$PHS_tp
  
  safe_apply(ncol(depths), function(c) {
    idx <- which(depths[, c] <= epi[c])
    calc_tli3(
      mean(chla[idx, c], na.rm = TRUE),
      mean(tn[idx, c], na.rm = TRUE),
      mean(tp[idx, c], na.rm = TRUE)
    )
  })
}

#' TLI 4 calculation function
#' @noRd
calc_LKE_tli4 <- function(out_list, hyps) {
  req_vars <- c("PHY_tchla", "NIT_tn", "PHS_tp", "LKE_photic",
                 "LKE_depths", "HYD_epidep")
  var_check <- check_vars(out_list, req_vars)
  if (!var_check) {
    return(NULL)
  }
  depths <- out_list$LKE_depths
  epi    <- out_list$HYD_epidep
  chla   <- out_list$PHY_tchla
  tn     <- out_list$NIT_tn
  tp     <- out_list$PHS_tp
  secchi <- out_list$LKE_photic
  
  safe_apply(ncol(depths), function(c) {
    idx <- which(depths[, c] <= epi[c])
    calc_tli4(
      mean(chla[idx, c], na.rm = TRUE),
      mean(tn[idx, c], na.rm = TRUE),
      mean(tp[idx, c], na.rm = TRUE),
      secchi[c]
    )
  })
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
