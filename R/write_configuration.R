#' Write model configuration from the aeme object
#'
#' @inheritParams build_aeme
#'
#' @return aeme object which was passed to the function,
#' @export

write_configuration <- function(aeme, model, path) {
  
  aeme  <- check_aeme(aeme)
  model <- if (missing(model)) list_models(aeme) else check_model(model)
  path  <- check_path(path, create = TRUE)
  lake_dir <- get_lake_dir(aeme, path)
  lke <- lake(aeme)
  name <- tolower(lke$name)
  model_config <- configuration(aeme)
  
  writers <- list(
    dy_cd    = write_config_dy_cd,
    glm_aed  = write_config_glm_aed,
    gotm_wet = write_config_gotm_wet
  )

  lapply(model, function(m) {
    if (m %in% names(writers)) {
      writers[[m]](
        model_config = model_config[[m]],
        model_dir = file.path(lake_dir, m),
        name = name
      )
    }
  })
  
  return(aeme)
}

#' Write DYRESM-CAEDYM configuration
#'
#' @inheritParams build_aeme
#'
#' @return write DYRESM config files to disk
#' @noRd
write_config_dy_cd <- function(model_config, model_dir, name) {

  model_dir <- check_path(model_dir, create = TRUE)
  if (is.null(model_config[["hydrodynamic"]]))
    cli::cli_abort("No DYRESM hydrodynamic configuration present")
  par_file <- file.path(model_dir, "dyresm3p1.par")
  writeLines(model_config$hydrodynamic$par, par_file)

  cfg_file <- file.path(model_dir, paste0(name, ".cfg"))
  writeLines(model_config$hydrodynamic$cfg, cfg_file)

  if (!is.null(model_config[["bgc"]])) {
    con_file <- file.path(model_dir, paste0(name, ".con"))
    writeLines(model_config$bgc$con, con_file)

    # Write CAEDYM bio file
    bio_file <- file.path(model_dir, "caedym3p1.bio")
    writeLines(model_config$bgc$bio, bio_file)

    # Write CAEDYM chm file
    chm_file <- file.path(model_dir, "caedym3p1.chm")
    writeLines(model_config$bgc$chm, chm_file)

    # Write CAEDYM sed file
    sed_file <- file.path(model_dir, "caedym3p1.sed")
    writeLines(model_config$bgc$sed, sed_file)

  }
  invisible()
}

#' Write GLM-AED configuration
#'
#' @inheritParams build_aeme
#'
#' @return write GLM config files to disk
#' @noRd
write_config_glm_aed <- function(model_config, model_dir, name) {

  model_dir <- check_path(model_dir, create = TRUE)
  if (is.null(model_config[["hydrodynamic"]]))
    cli::cli_abort("No GLM hydrodynamic configuration present")
  nml_file <- file.path(model_dir, "glm3.nml")
  write_nml(glm_nml = model_config$hydrodynamic, nml_file)

  if (!is.null(model_config[["bgc"]])) {
    # aed_dir <- file.path(model_dir, "aed2")
    # if (!dir.exists(aed_dir)) dir.create(aed_dir, recursive = TRUE)
    # 
    # # Write AED2 nml file
    # if (!is.null(model_config[["bgc"]][["aed"]])) {
    #   aed_file <- file.path(aed_dir, "aed2.nml")
    #   write_nml(glm_nml = model_config$bgc$aed, aed_file)
    # }
    # 
    # # Write AED2 phyto pars file
    # if (!is.null(model_config[["bgc"]][["phyto"]])) {
    #   phyto_file <- file.path(aed_dir, "aed2_phyto_pars.nml")
    #   write_nml(glm_nml = model_config$bgc$phyto, phyto_file)
    # }
    # 
    # # Write AED2 zoop pars file
    # if (!is.null(model_config[["bgc"]][["zoop"]])) {
    #   zoop_file <- file.path(aed_dir, "aed2_zoop_pars.nml")
    #   write_nml(glm_nml = model_config$bgc$zoop, zoop_file)
    # }
    aed_dir <- file.path(model_dir, "aed")
    aed_dir <- check_path(aed_dir, create = TRUE)
    if (!is.null(model_config[["bgc"]][["aed"]])) {
      aed_file <- file.path(aed_dir, "aed.nml")
      write_nml(glm_nml = model_config$bgc$aed, aed_file)
    }
    if (!is.null(model_config[["bgc"]][["aed_phyto_pars"]])) {
      phyto_file <- file.path(aed_dir, "aed_phyto_pars.csv")
      write_aed_param_csv(df = model_config$bgc$aed_phyto_pars,
                          file = phyto_file)
    }
    if (!is.null(model_config[["bgc"]][["aed_zoop_pars"]])) {
      zoop_file <- file.path(aed_dir, "aed_zoop_pars.csv")
      write_aed_param_csv(df = model_config$bgc$aed_zoop_pars,
                          file = zoop_file)
    }
    if (!is.null(model_config[["bgc"]][["aed_macrophyte_pars"]])) {
      macrophyte_file <- file.path(aed_dir, "aed_macrophyte_pars.csv")
      write_aed_param_csv(df = model_config$bgc$aed_macrophyte_pars,
                          file = macrophyte_file)
    }
  }
  invisible()
}

#' Write GOTM-WET configuration
#'
#' @inheritParams build_aeme
#'
#' @return write GOTM config files to disk
#' @noRd

write_config_gotm_wet <- function(model_config, model_dir, name) {

  model_dir <- check_path(model_dir, create = TRUE)
  if (is.null(model_config[["hydrodynamic"]]))
    cli::cli_abort("No GOTM hydrodynamic configuration present")
  write_yaml(model_config[["hydrodynamic"]][["gotm"]],
             file.path(model_dir, "gotm.yaml"))
  write_yaml(model_config[["hydrodynamic"]][["output"]],
             file.path(model_dir, "output.yaml"))

  if (!is.null(model_config[["bgc"]])) {
    fabm_file <- file.path(model_dir, "fabm.yaml")
    write_yaml(model_config[["bgc"]][["fabm"]], fabm_file)
  }
  invisible()
}

