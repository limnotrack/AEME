#' Load model configuration to the aeme object
#'
#' @inheritParams build_ensemble
#'
#' @return Updated aeme object with model configuration
#' @export
#'
#'

load_configuration <- function(model, aeme_data, path, use_bgc = FALSE) {

  lke <- lake(aeme_data)
  get_config_args <- list(lake = lke, path = path, use_bgc = use_bgc)
  model_config <- setNames(
    lapply(model, function(m) do.call(paste0("get_config_", m),
                                             get_config_args)),
    model
  )
  out <- list(physical = list(dy_cd = model_config[["dy_cd"]][["physical"]],
                              glm_aed = model_config[["glm_aed"]][["physical"]],
                              gotm_wet =
                                model_config[["gotm_wet"]][["physical"]]),
              bgc = list(dy_cd = model_config[["dy_cd"]][["bgc"]],
                         glm_aed = model_config[["glm_aed"]][["bgc"]],
                         gotm_wet = model_config[["gotm_wet"]][["bgc"]]))

  configuration(aeme_data) <- out
  return(aeme_data)

}


#' Get DYRESM-CAEDYM configuration
#'
#' @param lake list obtained from `lake(aeme_data)`
#' @inheritParams build_ensemble
#'
#' @return list of physical and bgc model configurations
#' @noRd
get_config_dy_cd <- function(lake, path, use_bgc) {

  lake_dir <- file.path(path, paste0(lake$id, "_", tolower(lake$name)))
  name <- tolower(lake$name)
  out <- list(physical = NULL, bgc = NULL)
  par_file <- file.path(lake_dir, "dy_cd", "dyresm3p1.par")
  if (!file.exists(par_file)) {
    stop("No DYRESM par file present at\n", par_file)
  }
  par <- readLines(par_file)
  cfg_file <- file.path(lake_dir, "dy_cd", paste0(name, ".cfg"))
  if (!file.exists(cfg_file)) {
    stop("No DYRESM cfg file present at\n", cfg_file)
  }
  cfg <- readLines(cfg_file)
  out$physical = list(par = par, cfg = cfg)

  if (use_bgc) {
    con_file <- file.path(lake_dir, "dy_cd", paste0(name, ".con"))
    if (!file.exists(con_file)) {
      stop("No DYRESM con file present at\n", con_file)
    }
    out$bgc <- readLines(con_file)
  }
  return(out)
}

#' Get GLM-AED configuration
#'
#' @param lake list obtained from `lake(aeme_data)`
#' @inheritParams build_ensemble
#'
#' @return list of physical and bgc model configurations
#' @noRd
get_config_glm_aed <- function(lake, path, use_bgc) {

  lake_dir <- file.path(path, paste0(lake$id, "_", tolower(lake$name)))
  out <- list(physical = NULL, bgc = NULL)
  nml_file <- file.path(lake_dir, "glm_aed", "glm3.nml")
  if (!file.exists(nml_file)) {
    stop("No GLM nml file present at\n", nml_file)
  }
  out$physical <- glmtools::read_nml(nml_file = nml_file)

  if (use_bgc) {
    aed_file <- file.path(lake_dir, "glm_aed", "aed2", "aed2.nml")
    if (!file.exists(aed_file)) {
      stop("No GLM-AED nml file present at\n", aed_file)
    }
    aed <- glmtools::read_nml(aed_file)
    phyto_file <- file.path(lake_dir, "glm_aed", "aed2", "aed2_phyto_pars.nml")
    if (!file.exists(phyto_file)) {
      stop("No GLM-AED nml file present at\n", phyto_file)
    }
    phyto <- glmtools::read_nml(phyto_file)

    # Zooplankton
    zoop_file <- file.path(lake_dir, "glm_aed", "aed2", "aed2_zoop_pars.nml")
    if (!file.exists(zoop_file)) {
      stop("No GLM-AED nml file present at\n", zoop_file)
    }
    zoop <- glmtools::read_nml(zoop_file)

    out$bgc <- list(aed = aed, phyto = phyto, zoop = zoop)
  }

  return(out)
}

#' Get GOTM-WET configuration
#'
#' @param lake list obtained from `lake(aeme_data)`
#' @inheritParams build_ensemble
#'
#' @return list of physical and bgc model configurations
#' @noRd
get_config_gotm_wet <- function(lake, path, use_bgc) {

  lake_dir <- file.path(path, paste0(lake$id, "_", tolower(lake$name)))
  out <- list(physical = NULL, bgc = NULL)
  yaml_file <- file.path(lake_dir, "gotm_wet", "gotm.yaml")
  if (!file.exists(yaml_file)) {
    stop("No GOTM yaml file present at\n", yaml_file)
  }
  out$physical <- gotmtools::read_yaml(file = yaml_file)

  if (use_bgc) {
    fabm_file <- file.path(lake_dir, "gotm_wet", "fabm.yaml")
    if (!file.exists(fabm_file)) {
      stop("No GOTM-FABM yaml file present at\n", fabm_file)
    }
    out$bgc <- gotmtools::read_yaml(file = fabm_file)
  }
  return(out)
}

