#' Setup AED aed_sed_const2d sediment model parameters
#'
#' @inheritParams build_aeme
#' @param baseline     Named numeric vector of baseline fluxes at
#'   \code{ref_depth}. Must include \code{fsed_oxy}, \code{fsed_amm},
#'   \code{fsed_nit}, \code{fsed_frp}.
#'
#' @returns Invisible NULL. Updates the aed.nml file in the glm_aed model
#'  directory.
#' @export
#'

set_aed_sed_const2d <- function(aeme, path, lake_dir = NULL,
                                baseline  = c(fsed_oxy = -25,
                                              fsed_amm =  2,
                                              fsed_nit =  0.2,
                                              fsed_frp =  0.05)) {
  
  if (is.null(lake_dir)) {
    if (missing(aeme)) {
      cli::cli_abort("Either {.arg lake_dir} or {.arg aeme} must be provided.")
    }
    if (missing(path)) {
      path <- get_aeme_path(aeme)
    }
    lake_dir <- get_lake_dir(aeme = aeme, path = path)
  }
  n_zones <- get_glm_sed_zones(aeme = aeme, lake_dir = lake_dir)
  model_config <- read_model_config(model = "glm_aed", lake_dir = lake_dir)
  
  cli_inform_safe(c("i" = paste0("Setting up AED aed_sed_const2d sediment
                                 zones: ", n_zones)))
  
  fluxes <- estimate_zone_fluxes(aeme = aeme, path = path, baseline = baseline)
  
  # Update aed_sed_const2d parameters in aed.nml
  model_config$bgc$aed$aed_sed_const2d$n_zones <- n_zones
  model_config$bgc$aed$aed_sed_const2d$active_zones <- seq_len(n_zones)
  model_config$bgc$aed$aed_sed_const2d$fsed_oxy <- fluxes$fsed_oxy
  model_config$bgc$aed$aed_sed_const2d$fsed_amm <- fluxes$fsed_amm
  model_config$bgc$aed$aed_sed_const2d$fsed_nit <- fluxes$fsed_nit
  model_config$bgc$aed$aed_sed_const2d$fsed_frp <- fluxes$fsed_frp
  
  model_dir <- file.path(lake_dir, "glm_aed")
  
  write_config_glm_aed(model_config = model_config, model_dir = model_dir)
  
  cfg <- configuration(aeme)
  cfg[["glm_aed"]] <- model_config
  configuration(aeme) <- cfg
  
  return(invisible(aeme))
}
