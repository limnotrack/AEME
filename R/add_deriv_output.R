#' Add derived output variables to model output
#'
#' @param out_list list of model output variables
#' @param hyps data frame with hypsograph data
#' @param vars_sim character vector of variable names to add. If NULL, all variables
#' with registered functions will be added. If not NULL, only the specified variables
#' will be added, but their dependencies will also be added.
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

#' Water density calculation function
#' @noRd
calc_HYD_dens <- function(out_list, hyps) {
  req_vars <- c("HYD_temp", "CHM_salt")
  var_check <- check_vars(out_list, req_vars)
  if (!var_check) {
    return(NULL)
  }
  wtr    <- out_list[["HYD_temp"]]
  salt <- out_list[["CHM_salt"]]
  
  safe_apply(ncol(wtr), function(c) {
    if (all(is.na(wtr[, c]))) return(NA_real_)
    v <- rLakeAnalyzer::water.density(wtr[, c], sal = salt[, c])
    v
  })
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

#' Depth-resolved internal energy content (energy density) calculation function
#'
#' Per-layer internal energy density (J/m3) at the model's own native depth
#' grid, i.e. the same shape as `HYD_temp`/`LKE_depths` (one row per depth
#' layer, one column per timestep). Deliberately mirrors
#' `rLakeAnalyzer::internal.energy()`'s own density convention
#' (`water.density(wtr)`, temperature only, no salinity term) so this and
#' `calc_LKE_nrgtot()` stay internally consistent/summable with each other,
#' rather than reusing `calc_HYD_dens()` (which also folds in `CHM_salt`).
#' @noRd
calc_HYD_nrgcnt <- function(out_list, hyps) {
  req_vars <- c("HYD_temp")
  var_check <- check_vars(out_list, req_vars)
  if (!var_check) {
    return(NULL)
  }
  wtr <- out_list[["HYD_temp"]]
  cw  <- 4186  # J/(kg degC), matches rLakeAnalyzer::internal.energy()

  rho <- matrix(rLakeAnalyzer::water.density(as.vector(wtr)),
                nrow = nrow(wtr), ncol = ncol(wtr))
  rho * cw * wtr
}

#' Lake total internal energy (heat content) calculation function
#' @noRd
calc_LKE_nrgtot <- function(out_list, hyps) {
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

    v <- internal_energy_total(wtr = wtr[, c], depths = depths[, c],
                               bthA = bthA, bthD = bthD)
    if (is.nan(v)) NA_real_ else v
  })
}

#' Whole-lake internal energy in Joules (a real total, not per unit area).
#'
#' Adapted from `rLakeAnalyzer::internal.energy()` with its final
#' area-normalisation step (`U <- sum(u_i) / layerA[1]`) removed, so the
#' result is total heat content rather than an areal energy density -
#' matching what "lake total energy" means, as distinct from the
#' depth-resolved `HYD_nrgcnt` energy-density profile above.
#' @noRd
internal_energy_total <- function(wtr, depths, bthA, bthD) {
  dz <- 0.1
  cw <- 4186
  if (min(bthD) < 0) {
    useI <- bthD >= 0
    depT <- if (any(bthD == 0)) bthD[useI] else c(0, bthD[useI])
    bthA <- approx(bthD, bthA, depT)$y
    bthD <- depT
  }
  numD <- length(wtr)
  if (max(bthD) > depths[numD]) {
    wtr[numD + 1] <- wtr[numD]
    depths[numD + 1] <- max(bthD)
  } else if (max(bthD) < depths[numD]) {
    bthD <- c(bthD, depths[numD])
    bthA <- c(bthA, 0)
  }
  if (min(bthD) < depths[1]) {
    wtr <- c(wtr[1], wtr)
    depths <- c(min(bthD), depths)
  }
  Io <- which.min(depths)
  Ao <- bthA[Io]
  if (length(Ao) == 0 || is.na(Ao) || Ao == 0) return(NA_real_)

  rhoL   <- rLakeAnalyzer::water.density(wtr)
  layerD <- seq(min(depths), max(depths), by = dz)
  layerP <- approx(depths, rhoL, layerD)$y
  layerT <- approx(depths, wtr, layerD)$y
  layerA <- approx(bthD, bthA, layerD)$y
  v_i <- layerA * dz
  m_i <- layerP * v_i
  u_i <- layerT * m_i * cw
  sum(u_i)
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
    idx <- epi_idx(depths[, c], epi[c])
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

#' Dissolved oxygen percent saturation calculation function
#' @noRd
calc_CHM_oxysat <- function(out_list, hyps) {
  req_vars <- c("CHM_oxy", "HYD_temp", "LKE_depths")
  var_check <- check_vars(out_list, req_vars)
  if (!var_check) {
    return(NULL)
  }
  oxy    <- out_list[["CHM_oxy"]]
  wtr    <- out_list[["HYD_temp"]]
  depths <- out_list[["LKE_depths"]]

  convert_do(value = oxy, temp = wtr, depth = depths, direction = "to_percent")
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

#' Depth indices at or above the epilimnion depth, for one output column.
#'
#' `which(depths <= epi)` is occasionally empty - e.g. epi reported as 0 or
#' slightly negative at the exact moment stratification sets up/breaks down,
#' or a depth grid that starts below a very shallow epi - which makes the
#' caller's `mean(x[idx], na.rm = TRUE)` return NaN regardless of `na.rm`
#' (mean() of a zero-length vector is NaN, not NA). NaN then fails any
#' downstream is.finite() check the same way -Inf does, aborting an entire
#' PEST++ forward run over what is really just a single edge-case day.
#' Falling back to the single shallowest sampled depth keeps every day
#' contributing a real (if less precise) TLI value instead of none at all.
#' @noRd
epi_idx <- function(depths_col, epi_val) {
  idx <- which(depths_col <= epi_val)
  if (length(idx) == 0) idx <- which.min(depths_col)
  idx
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
    idx <- epi_idx(depths[, c], epi[c])
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
    idx <- epi_idx(depths[, c], epi[c])
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
    idx <- epi_idx(depths[, c], epi[c])
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
    idx <- epi_idx(depths[, c], epi[c])
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
    idx <- epi_idx(depths[, c], epi[c])
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
