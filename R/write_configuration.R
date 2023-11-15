#' Write model configuration from the aeme object
#'
#' @inheritParams build_ensemble
#'
#' @return aeme object which was passed to the function,
#' @export

write_configuration <- function(model, aeme_data, path) {

  if (!dir.exists(path)) dir.create(path)

  lke <- lake(aeme_data)
  get_config_args <- list(aeme_data = aeme_data, path = path)
  lapply(model, \(m) do.call(paste0("write_config_", m),
                                      get_config_args))
  aeme_data
}

#' Write DYRESM-CAEDYM configuration
#'
#' @inheritParams build_ensemble
#'
#' @return write DYRESM config files to disk
#' @noRd
write_config_dy_cd <- function(aeme_data, path) {

  lke <- lake(aeme_data)
  name <- tolower(lke$name)
  model_dir <- file.path(path, paste0(lke$id, "_", tolower(lke$name)), "dy_cd")
  if (!dir.exists(model_dir)) dir.create(model_dir, recursive = TRUE)
  model_config <- configuration(aeme_data)
  if (is.null(model_config[["dy_cd"]][["hydrodynamic"]]))
    stop("No DYRESM hydrodynamic configuration present")
  par_file <- file.path(model_dir, "dyresm3p1.par")
  writeLines(model_config$dy_cd$hydrodynamic$par, par_file)

  cfg_file <- file.path(model_dir, paste0(name, ".cfg"))
  writeLines(model_config$dy_cd$hydrodynamic$cfg, cfg_file)

  if (!is.null(model_config[["dy_cd"]][["ecosystem"]])) {
    con_file <- file.path(model_dir, paste0(name, ".con"))
    writeLines(model_config$dy_cd$ecosystem$con, con_file)

    # Write CAEDYM bio file
    bio_file <- file.path(model_dir, "caedym3p1.bio")
    writeLines(model_config$dy_cd$ecosystem$bio, bio_file)

    # Write CAEDYM chm file
    chm_file <- file.path(model_dir, "caedym3p1.chm")
    writeLines(model_config$dy_cd$ecosystem$chm, chm_file)

    # Write CAEDYM sed file
    sed_file <- file.path(model_dir, "caedym3p1.sed")
    writeLines(model_config$dy_cd$ecosystem$sed, sed_file)

  }
  invisible()
}

#' Write GLM-AED configuration
#'
#' @inheritParams build_ensemble
#'
#' @return write GLM config files to disk
#' @noRd
write_config_glm_aed <- function(aeme_data, path) {

  lke <- lake(aeme_data)
  model_dir <- file.path(path, paste0(lke$id, "_", tolower(lke$name)),
                         "glm_aed")

  if (!dir.exists(model_dir)) dir.create(model_dir, recursive = TRUE)
  model_config <- configuration(aeme_data)
  if (is.null(model_config[["glm_aed"]][["hydrodynamic"]]))
    stop("No GLM hydrodynamic configuration present")
  nml_file <- file.path(model_dir, "glm3.nml")
  write_nml(glm_nml = model_config$glm_aed$hydrodynamic, nml_file)

  if (!is.null(model_config[["glm_aed"]][["ecosystem"]])) {
    aed_dir <- file.path(model_dir, "aed2")
    if (!dir.exists(aed_dir)) dir.create(aed_dir, recursive = TRUE)

    # Write AED2 nml file
    if (!is.null(model_config[["glm_aed"]][["ecosystem"]][["aed"]])) {
      aed_file <- file.path(aed_dir, "aed2.nml")
      write_nml(glm_nml = model_config$glm_aed$ecosystem$aed, aed_file)
    }

    # Write AED2 phyto pars file
    if (!is.null(model_config[["glm_aed"]][["ecosystem"]][["phyto"]])) {
      phyto_file <- file.path(aed_dir, "aed2_phyto_pars.nml")
      write_nml(glm_nml = model_config$glm_aed$ecosystem$phyto, phyto_file)
    }

    # Write AED2 zoop pars file
    if (!is.null(model_config[["glm_aed"]][["ecosystem"]][["zoop"]])) {
      zoop_file <- file.path(aed_dir, "aed2_zoop_pars.nml")
      write_nml(glm_nml = model_config$glm_aed$ecosystem$zoop, zoop_file)
    }
  }
  invisible()
}

#' Write GOTM-WET configuration
#'
#' @inheritParams build_ensemble
#'
#' @return write GOTM config files to disk
#' @noRd

write_config_gotm_wet <- function(aeme_data, path) {

  lke <- lake(aeme_data)
  model_dir <- file.path(path, paste0(lke$id, "_", tolower(lke$name)),
                         "gotm_wet")

  if (!dir.exists(model_dir)) dir.create(model_dir, recursive = TRUE)
  model_config <- configuration(aeme_data)
  if (is.null(model_config[["gotm_wet"]][["hydrodynamic"]]))
    stop("No GOTM hydrodynamic configuration present")
  write_yaml(model_config[["gotm_wet"]][["hydrodynamic"]][["gotm"]],
             file.path(model_dir, "gotm.yaml"))
  write_yaml(model_config[["gotm_wet"]][["hydrodynamic"]][["output"]],
             file.path(model_dir, "output.yaml"))

  if (!is.null(model_config[["gotm_wet"]][["ecosystem"]])) {
    fabm_file <- file.path(model_dir, "fabm.yaml")
    write_yaml(model_config[["gotm_wet"]][["ecosystem"]], fabm_file)
  }
  invisible()
}

