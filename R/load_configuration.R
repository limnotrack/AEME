#' Load model configuration to the aeme object
#'
#' @inheritParams build_aeme
#'
#' @return Updated aeme object with model configuration
#' @export
#'

load_configuration <- function(aeme, 
                               model,
                               path = ".",
                               model_controls = NULL, 
                               use_bgc = FALSE, 
                               ext_elev = 0,
                               calc_wbal = TRUE,
                               wb_method = 2,
                               calc_wlev = TRUE,
                               use_aeme = FALSE,
                               coeffs = NULL,
                               hum_type = 3,
                               est_swr_hr = TRUE) {

  aeme <- check_aeme(aeme)
  if (missing(model)) {
    model <- list_models(aeme)
  } else {
    model <- check_model(model = model)
  }
  if (missing(path)) {
    path <- get_aeme_path(aeme)
  }
  path <- check_path(path = path, must_exist = TRUE)
  if (is.null(model_controls)) {
    model_controls <- get_model_controls(aeme = aeme)
  }
  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  get_config_args <- list(path = lake_dir)
  model_config <- setNames(
    lapply(model, read_model_config,
           lake_dir = lake_dir),
    model
  )

  out <- list(model_controls = model_controls,
              use_bgc = use_bgc,
              path = path,
              aeme_version = as.character(utils::packageVersion("AEME")),
              ext_elev = ext_elev,
              calc_wbal = calc_wbal, wb_method = wb_method, 
              calc_wlev = calc_wlev,
              coeffs = coeffs, hum_type = hum_type,
              est_swr_hr = est_swr_hr,
              dy_cd = list(hydrodynamic = model_config[["dy_cd"]][["hydrodynamic"]],
                           bgc = model_config[["dy_cd"]][["bgc"]]),
              glm_aed = list(hydrodynamic =
                               model_config[["glm_aed"]][["hydrodynamic"]],
                             bgc = model_config[["glm_aed"]][["bgc"]]),
              gotm_wet = list(hydrodynamic =
                                model_config[["gotm_wet"]][["hydrodynamic"]],
                              bgc = model_config[["gotm_wet"]][["bgc"]]),
              simstrat_aed2 = list(hydrodynamic =
                                     model_config[["simstrat_aed2"]][["hydrodynamic"]],
                                   bgc = model_config[["simstrat_aed2"]][["bgc"]])
  )

  configuration(aeme) <- out
  aeme
}


#' Get DYRESM-CAEDYM configuration
#'
#' @param lake list obtained from `lake(aeme)`
#' @inheritParams build_aeme
#'
#' @return list of hydrodynamic and bgc model configurations
#' @noRd
get_config_dy_cd <- function(lake_dir, path) {

  name <- tolower(lake$name)
  out <- list(hydrodynamic = NULL, bgc = NULL)
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
  out$hydrodynamic = list(par = par, cfg = cfg)

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
#' @return list of hydrodynamic and bgc model configurations
#' @noRd
get_config_glm_aed <- function(lake_dir, path) {

  out <- list(hydrodynamic = NULL, bgc = NULL)
  nml_file <- file.path(lake_dir, "glm_aed", "glm3.nml")
  if (!file.exists(nml_file)) {
    stop("No GLM nml file present at\n", nml_file)
  }

  model_cfg_files <- get_model_config_files(path = lake_dir,
                                            model = "glm_aed")[["glm_aed"]]
  cfg <- lapply(model_cfg_files, \(f) {
    file_type <- tools::file_ext(f)
    if (file_type == "nml") {
      read_nml(f)
    } else if (file_type %in% c("csv", "tsv")) {
      read_aed_param_csv(f)
    }
  })
  out$hydrodynamic <- cfg[["glm3"]]
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
#' @return list of hydrodynamic and bgc model configurations
#' @noRd
get_config_gotm_wet <- function(lake_dir, path) {

  out <- list(hydrodynamic = NULL, bgc = NULL)
  yaml_file <- file.path(lake_dir, "gotm_wet", "gotm.yaml")
  if (!file.exists(yaml_file)) {
    stop("No GOTM yaml file present at\n", yaml_file)
  }
  out[["hydrodynamic"]][["gotm"]] <- yaml::read_yaml(file = yaml_file)

  yaml_file <- file.path(lake_dir, "gotm_wet", "output.yaml")
  if (!file.exists(yaml_file)) {
    stop("No GOTM output yaml file present at\n", yaml_file)
  }
  suppressWarnings({
    out[["hydrodynamic"]][["output"]] <- yaml::read_yaml(file = yaml_file)
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

