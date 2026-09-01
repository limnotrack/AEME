#' Setup AED aed_sed_const2d sediment model parameters
#'
#' @inheritParams build_aeme
#' @param lake_dir Path to the lake AEME directory. If `NULL`, it is derived
#' from `aeme`/`path`.
#' @param baseline     Named numeric vector of baseline fluxes at
#'   \code{ref_depth}. Must include \code{fsed_oxy}, \code{fsed_amm},
#'   \code{fsed_nit}, \code{fsed_frp}.
#'
#' @details
#' Any per-zone flux (`fsed_oxy` / `fsed_amm` / `fsed_nit` / `fsed_frp`) that is
#' already present in `parameters(aeme)` as an `aed_sed_const2d/...` row is left
#' untouched - it is applied later by [input_model_parameters()]. The zone-flux
#' estimation ([estimate_zone_fluxes()]) still runs so its diagnostic tables are
#' printed, but its values for those keys are not written.
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

  # Per-zone fluxes the user has pinned via parameters(aeme) are applied by
  # input_model_parameters() after this; don't overwrite them here.
  flux_keys <- c("fsed_oxy", "fsed_amm", "fsed_nit", "fsed_frp")
  prm <- tryCatch(parameters(aeme = aeme), error = function(e) NULL)
  pinned <- character(0)
  if (is.data.frame(prm) && nrow(prm) > 0 &&
      all(c("model", "name") %in% names(prm))) {
    pinned <- flux_keys[paste0("aed_sed_const2d/", flux_keys) %in%
                          prm$name[prm$model == "glm_aed"]]
  }

  cli_inform_safe(c("i" = paste0("Setting up AED aed_sed_const2d sediment
                                 zones: ", n_zones)))

  fluxes <- estimate_zone_fluxes(aeme = aeme, path = path, baseline = baseline)

  # Update aed_sed_const2d parameters in aed.nml
  model_config$bgc$aed$aed_sed_const2d$n_zones <- n_zones
  model_config$bgc$aed$aed_sed_const2d$active_zones <- seq_len(n_zones)
  for (k in setdiff(flux_keys, pinned)) {
    model_config$bgc$aed$aed_sed_const2d[[k]] <- fluxes[[k]]
  }
  if (length(pinned) > 0) {
    cli_inform_safe(c("i" = "Keeping user-supplied {.val {pinned}} from \\
                             {.code parameters(aeme)} (zone-flux estimate not \\
                             applied for {?it/them})."))
  }

  model_dir <- file.path(lake_dir, "glm_aed")
  
  write_config_glm_aed(model_config = model_config, model_dir = model_dir)
  
  cfg <- configuration(aeme)
  cfg[["glm_aed"]] <- model_config
  configuration(aeme) <- cfg
  
  return(invisible(aeme))
}
