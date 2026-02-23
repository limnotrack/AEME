#' Setup AED aed_totals parameters
#' 
#' This function sets up the aed_totals parameters in the aed block of the bgc 
#' section of the model config for glm_aed. It extracts the necessary 
#' information from the aed_phyto_pars and aed blocks to determine which 
#' variables to include in the totals for TN, TP and TOC, and their scaling 
#' factors. It then updates the model config and writes it back to the aed.nml 
#' file in the glm_aed model directory.
#'
#' @inheritParams build_aeme
#'
#' @returns Invisible NULL. Updates the aed.nml file in the glm_aed model
#'  directory.
#' @export
#'

set_aed_totals <- function(aeme, path, lake_dir = NULL) {
  
  if (is.null(lake_dir)) {
    if (missing(aeme) | missing(path)) {
      cli::cli_abort("Either {.arg lake_dir} or both {.arg aeme} and
                     {.arg path} must be provided.")
    }
    lake_dir <- get_lake_dir(aeme = aeme, path = path)
  }
  model_config <- read_model_config(model = "glm_aed", lake_dir = lake_dir)
  
  aed <- model_config[["bgc"]][["aed"]]
  if (is.null(aed)) {
    cli::cli_abort("The aed block in the bgc section is not present. 
                   Try re-building the model config with
                   {.code build_aeme(aeme = aeme, model = model, path = path)}.")
  }
  
  phyto_pars <- model_config[["bgc"]][["aed_phyto_pars"]]
  if (is.null(phyto_pars)) {
    cli::cli_abort("The aed_phyto_pars in the bgc section are not present. 
                   Try re-building the model config with
                   {.code build_aeme(aeme = aeme, model = model, path = path)}.")
  }
  
  # ---- Validate required rows ----
  required_rows <- c("X_ncon","X_pcon","simINDynamics","simIPDynamics")
  
  if (!all(required_rows %in% phyto_pars$p_name)) {
    cli::cli_abort(c(
      "aed_phyto_pars must contain rows:",
      "{.val {required_rows}}"
    ))
  }
  
  # ---- Extract phyto groups ----
  phy_groups <- setdiff(names(phyto_pars), "p_name")
  
  # Helper to extract a parameter row as numeric vector
  get_par <- function(par_name) {
    as.numeric(
      phyto_pars[phyto_pars$p_name == par_name, phy_groups]
    )
  }
  
  X_ncon <- get_par("X_ncon")
  X_pcon <- get_par("X_pcon")
  simINDynamics <- get_par("simINDynamics")
  simIPDynamics <- get_par("simIPDynamics")
  
  # ----------------------------------------------------
  # -------------------- TN ----------------------------
  # ----------------------------------------------------
  
  TN_vars  <- c("NIT_nit","NIT_amm","OGM_don","OGM_pon")
  TN_scale <- c(1,1,1,1)
  
  for (i in seq_along(phy_groups)) {
    
    g <- phy_groups[i]
    
    if (simINDynamics[i] > 0) {
      # Use internal quota pool
      TN_vars  <- c(TN_vars,  paste0("PHY_", g, "_IN"))
      TN_scale <- c(TN_scale, 1.0)
    } else {
      # Use biomass × fixed N:C
      TN_vars  <- c(TN_vars,  paste0("PHY_", g))
      TN_scale <- c(TN_scale, X_ncon[i])
    }
  }
  
  # ----------------------------------------------------
  # -------------------- TP ----------------------------
  # ----------------------------------------------------
  
  TP_vars  <- c("PHS_frp", "OGM_dop", "OGM_pop")
  TP_scale <- c(1, 1, 1)
  
  for (i in seq_along(phy_groups)) {
    
    g <- phy_groups[i]
    
    if (simIPDynamics[i] > 0) {
      TP_vars  <- c(TP_vars,  paste0("PHY_", g, "_IP"))
      TP_scale <- c(TP_scale, 1.0)
    } else {
      TP_vars  <- c(TP_vars,  paste0("PHY_", g))
      TP_scale <- c(TP_scale, X_pcon[i])
    }
  }
  
  # ----------------------------------------------------
  # -------------------- TOC ---------------------------
  # ----------------------------------------------------
  
  TOC_vars  <- c("OGM_doc","OGM_poc")
  TOC_scale <- c(1, 1)
  
  for (g in phy_groups) {
    TOC_vars  <- c(TOC_vars,  paste0("PHY_", g))
    TOC_scale <- c(TOC_scale, 1.0)
  }
  
  # ----------------------------------------------------
  # ------------- Format AED block ---------------------
  # ----------------------------------------------------
  
  aed_totals <- list(
    TN_vars = TN_vars,
    TN_varscale = TN_scale,
    TP_vars = TP_vars,
    TP_varscale = TP_scale,
    TOC_vars = TOC_vars,
    TOC_varscale = TOC_scale
  )
  
  model_config[["bgc"]][["aed"]][["aed_totals"]] <- aed_totals
  
  model_dir <- file.path(lake_dir, "glm_aed")
  
  write_config_glm_aed(model_config = model_config, model_dir = model_dir)
  
  return(invisible())
}
