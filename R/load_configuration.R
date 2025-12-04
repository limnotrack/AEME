#' Load model configuration to the aeme object
#'
#' @inheritParams build_aeme
#'
#' @return Updated aeme object with model configuration
#' @export
#'

load_configuration <- function(aeme, model, model_controls = NULL, 
                               use_bgc = FALSE, path) {

  aeme <- check_aeme(aeme)
  if (missing(model)) {
    model <- list_models(aeme)
  } else {
    model <- check_model(model = model)
  }
  if (is.null(model_controls)) {
    model_controls <- get_model_controls(aeme = aeme)
  }
  path <- check_path(path = path, must_exist = TRUE)
  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  get_config_args <- list(path = lake_dir)
  model_config <- setNames(
    lapply(model, read_model_config,
           path = lake_dir),
    model
  )
  
  use_bgc <- all(sapply(model_config, \(x) !is.null(x[["bgc"]])))
  
  out <- list(model_controls = model_controls,
              use_bgc = use_bgc,
              dy_cd = list(hydrodynamic = model_config[["dy_cd"]][["physical"]],
                           ecosystem = model_config[["dy_cd"]][["bgc"]]),
              glm_aed = list(hydrodynamic =
                               model_config[["glm_aed"]][["physical"]],
                             ecosystem = model_config[["glm_aed"]][["bgc"]]),
              gotm_wet = list(hydrodynamic =
                                model_config[["gotm_wet"]][["physical"]],
                              ecosystem = model_config[["gotm_wet"]][["bgc"]])
  )

  configuration(aeme) <- out
  aeme
}


#' Get DYRESM-CAEDYM configuration
#'
#' @param lake list obtained from `lake(aeme)`
#' @inheritParams build_aeme
#'
#' @return list of physical and bgc model configurations
#' @noRd
get_config_dy_cd <- function(lake_dir, path) {

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

  # Bio file
  bio_file <- file.path(lake_dir, "dy_cd", "caedym3p1.bio")
  use_bgc <- file.exists(bio_file)

  if (use_bgc) {

    # Con file
    con_file <- file.path(lake_dir, "dy_cd", paste0(name, ".con"))
    if (!file.exists(con_file)) {
      stop("No DYRESM con file present at\n", con_file)
    }
    out$bgc$con <- readLines(con_file)

    # Bio file
    bio_file <- file.path(lake_dir, "dy_cd", "caedym3p1.bio")
    if (!file.exists(bio_file)) {
      stop("No DYRESM bio file present at\n", bio_file)
    }
    out$bgc$bio <- readLines(bio_file)

    # Chm file
    chm_file <- file.path(lake_dir, "dy_cd", "caedym3p1.chm")
    if (!file.exists(chm_file)) {
      stop("No DYRESM chm file present at\n", chm_file)
    }
    out$bgc$chm <- readLines(chm_file)

    # Sed file
    sed_file <- file.path(lake_dir, "dy_cd", "caedym3p1.sed")
    if (!file.exists(sed_file)) {
      stop("No DYRESM sed file present at\n", sed_file)
    }
    out$bgc$sed <- readLines(sed_file)
  }
  return(out)
}

#' Get GLM-AED configuration
#'
#' @param lake list obtained from `lake(aeme)`
#' @inheritParams build_aeme
#'
#' @return list of physical and bgc model configurations
#' @noRd
get_config_glm_aed <- function(lake_dir, path) {

  out <- list(physical = NULL, bgc = NULL)
  nml_file <- file.path(lake_dir, "glm_aed", "glm3.nml")
  if (!file.exists(nml_file)) {
    stop("No GLM nml file present at\n", nml_file)
  }

  model_cfg_files <- get_model_config_files(lake_dir = lake_dir, 
                                            model = "glm_aed")[["glm_aed"]]
  cfg <- lapply(model_cfg_files, \(f) {
    file_type <- tools::file_ext(f)
    if (file_type == "nml") {
      read_nml(f)
    } else if (file_type %in% c("csv", "tsv")) {
      read_aed_param_csv(f)
    }
  })
  out$physical <- cfg[["glm3"]]
  # Remove glm3 from list
  cfg[["glm3"]] <- NULL
  if (length(cfg) > 0) {
    out$bgc <- cfg
  }
  return(out)
}

#' Get GOTM-WET configuration
#'
#' @param lake list obtained from `lake(aeme)`
#' @inheritParams build_aeme
#'
#' @return list of physical and bgc model configurations
#' @noRd
get_config_gotm_wet <- function(lake_dir, path) {

  out <- list(physical = NULL, bgc = NULL)
  yaml_file <- file.path(lake_dir, "gotm_wet", "gotm.yaml")
  if (!file.exists(yaml_file)) {
    stop("No GOTM yaml file present at\n", yaml_file)
  }
  out[["physical"]][["gotm"]] <- yaml::read_yaml(file = yaml_file)

  yaml_file <- file.path(lake_dir, "gotm_wet", "output.yaml")
  if (!file.exists(yaml_file)) {
    stop("No GOTM output yaml file present at\n", yaml_file)
  }
  suppressWarnings({
    out[["physical"]][["output"]] <- yaml::read_yaml(file = yaml_file)
  })

  fabm_file <- file.path(lake_dir, "gotm_wet", "fabm.yaml")
  use_bgc <- file.exists(fabm_file)

  if (use_bgc) {
    fabm_file <- file.path(lake_dir, "gotm_wet", "fabm.yaml")
    if (!file.exists(fabm_file)) {
      stop("No GOTM-FABM yaml file present at\n", fabm_file)
    }
    out$bgc <- yaml::read_yaml(file = fabm_file)
  }
  return(out)
}

