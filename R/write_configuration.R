#' Write model configuration from the aeme object
#'
#' Writes each requested model's configuration files straight from the
#' `aeme` object's cached state, with no recomputation of any kind -- the
#' hydrodynamic/bgc files come verbatim from `configuration(aeme)`, and (when
#' `include_boundary = TRUE`) the meteorology/inflow/outflow
#' boundary-condition files come straight from `input(aeme)`/`inflows(aeme)`/
#' `outflows(aeme)`, bypassing [build_aeme()]'s water-balance/lake-level/
#' AED-re-derivation pipeline entirely. This makes it the safe choice for
#' rewriting an already-built (or [glm_config_to_aeme()]-loaded)
#' configuration to disk unchanged -- e.g. into a fresh directory -- without
#' the risk of `build_aeme(use_aeme = TRUE)` silently regenerating values
#' from generic state instead of trusting what's cached.
#'
#' @inheritParams build_aeme
#' @param path character; path to the directory where the model configuration
#'   should be written. Default is the current working directory.
#' @param include_boundary logical; also write each model's boundary-condition
#'   files (meteorology, inflows and outflows, e.g. GLM-AED's
#'   `bcs/meteo_glm.csv`, `bcs/inflow_*.csv` and `bcs/outflow_*.csv`) in the
#'   model's own format, straight from `input(aeme)`/`inflows(aeme)`/
#'   `outflows(aeme)`. Default `TRUE`.
#'
#' @param apply_params logical; apply `parameters(aeme)` when writing. The
#'   configuration-file parameters are set on the configuration being written,
#'   and (for the boundary files) the met/inflow/outflow scaling parameters
#'   are applied to the data being written. Neither `configuration(aeme)` nor
#'   `input(aeme)`/`inflows(aeme)`/`outflows(aeme)` is changed: the parameters
#'   only affect what reaches disk. Parameters for a file the configuration
#'   does not have (e.g. bgc parameters for a model built without bgc) are
#'   skipped with a warning. Default `TRUE`.
#'
#' @return aeme object which was passed to the function,
#' @export

write_configuration <- function(aeme, model, path = getwd(),
                                include_boundary = TRUE,
                                apply_params = TRUE) {

  aeme  <- check_aeme(aeme)
  model <- if (missing(model)) list_models(aeme) else check_model(model)
  path  <- check_path(path, create = TRUE)
  lake_dir <- get_lake_dir(aeme, path)
  lke <- lake(aeme)
  name <- tolower(lke$name)
  model_config <- configuration(aeme)

  param <- if (apply_params) usable_parameters(aeme) else NULL
  if (!is.null(param)) {
    for (m in intersect(model, unique(param$model))) {
      if (is.null(model_config[[m]])) next
      p_m <- param[param$model == m, , drop = FALSE]
      if (!any(!p_m$file %in% c("met", "inf", "wdr"))) next
      model_config[[m]] <- apply_parameters(model_config[[m]], p_m, m,
                                            strict = FALSE)$config
    }
  }

  writers <- list(
    dy_cd    = write_config_dy_cd,
    glm_aed  = write_config_glm_aed,
    gotm_wet = write_config_gotm_wet,
    simstrat_aed2 = write_config_simstrat_aed2,
    simstrat_aed  = write_config_simstrat_aed
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

  if (include_boundary) {
    inp <- input(aeme)
    for (m in model) {
      if (is.null(model_config[[m]])) next
      p_m <- if (!is.null(param)) param[param$model == m, , drop = FALSE]
      # Scaling parameters give scaled copies of the data to write; the data
      # held in `aeme` is not changed
      bnd <- if (!is.null(p_m) && nrow(p_m) > 0) {
        apply_boundary_params(meteo = inp[["meteo"]],
                              outflows = outflows(aeme)[["data"]], param = p_m)
      } else {
        list()
      }
      write_boundary_data(
        aeme = aeme, model = m, lake_dir = lake_dir,
        meteo = bnd$meteo %||% inp[["meteo"]],
        outf = bnd$outflows %||% outflows(aeme)[["data"]],
        inf = inflows(aeme)[["data"]],
        inf_factor = bnd$inf_factor %||% 1
      )
    }
  }

  return(invisible(aeme))
}

#' Parameters in a form `apply_parameters()` can use
#'
#' @param aeme Aeme object.
#' @return `parameters(aeme)` with the columns `collapse_params()` requires,
#'   or `NULL` if there are no parameters.
#' @noRd
usable_parameters <- function(aeme) {
  param <- parameters(aeme)
  needed <- c("model", "file", "name", "value", "min", "max", "group")
  if (!is.data.frame(param) || nrow(param) == 0 ||
      !all(needed %in% names(param))) {
    return(NULL)
  }
  if (!"index" %in% names(param)) param$index <- NA_integer_
  collapse_params(param)
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
  # Prefer the GLM version this configuration was actually read from (set by
  # read_model_config()); fall back to whatever nml is already present in
  # model_dir, then finally to glm3.nml, so a config read from glm4.nml
  # doesn't get silently written back out as glm3.nml
  glm_nml_name <- model_config[["hydrodynamic_file"]]
  if (is.null(glm_nml_name)) {
    existing <- find_glm_nml(model_dir, must_exist = FALSE)
    glm_nml_name <- if (!is.na(existing)) basename(existing) else "glm3.nml"
  }
  nml_file <- file.path(model_dir, glm_nml_name)
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

#' Write Simstrat-AED2 configuration
#'
#' @inheritParams build_aeme
#'
#' @return write Simstrat-AED2 config files to disk
#' @noRd
write_config_simstrat_aed2 <- function(model_config, model_dir, name) {

  model_dir <- check_path(model_dir, create = TRUE)
  if (is.null(model_config[["hydrodynamic"]]))
    cli::cli_abort("No Simstrat hydrodynamic configuration present")
  par_file <- file.path(model_dir, "simstrat.par")
  jsonlite::write_json(model_config[["hydrodynamic"]], par_file,
                       pretty = TRUE, auto_unbox = TRUE, null = "null")

  if (!is.null(model_config[["bgc"]])) {
    if (!is.null(model_config[["bgc"]][["aed2"]])) {
      write_nml(model_config[["bgc"]][["aed2"]], file.path(model_dir, "aed2.nml"))
    }
    if (!is.null(model_config[["bgc"]][["aed2_phyto_pars"]])) {
      write_nml(model_config[["bgc"]][["aed2_phyto_pars"]],
               file.path(model_dir, "aed2_phyto_pars.nml"))
    }
    if (!is.null(model_config[["bgc"]][["aed2_zoop_pars"]])) {
      write_nml(model_config[["bgc"]][["aed2_zoop_pars"]],
               file.path(model_dir, "aed2_zoop_pars.nml"))
    }
  }
  invisible()
}

#' Write Simstrat-AED configuration
#'
#' Mirrors \code{\link{write_config_simstrat_aed2}} for the Simstrat-AED
#' (not AED2) coupling -- AED's phyto/zoo/macrophyte par files are CSV, not
#' `%`-syntax nml, so those are written with \code{\link{write_aed_param_csv}}
#' (the same writer GLM-AED's config uses) instead of \code{\link{write_nml}}.
#'
#' @inheritParams build_aeme
#'
#' @return write Simstrat-AED config files to disk
#' @noRd
write_config_simstrat_aed <- function(model_config, model_dir, name) {

  model_dir <- check_path(model_dir, create = TRUE)
  if (is.null(model_config[["hydrodynamic"]]))
    cli::cli_abort("No Simstrat hydrodynamic configuration present")
  par_file <- file.path(model_dir, "simstrat.par")
  jsonlite::write_json(model_config[["hydrodynamic"]], par_file,
                       pretty = TRUE, auto_unbox = TRUE, null = "null")

  if (!is.null(model_config[["bgc"]])) {
    if (!is.null(model_config[["bgc"]][["aed"]])) {
      write_nml(model_config[["bgc"]][["aed"]], file.path(model_dir, "aed.nml"))
    }
    if (!is.null(model_config[["bgc"]][["aed_phyto_pars"]])) {
      write_aed_param_csv(model_config[["bgc"]][["aed_phyto_pars"]],
                          file.path(model_dir, "aed_phyto_pars.csv"))
    }
    if (!is.null(model_config[["bgc"]][["aed_zoop_pars"]])) {
      write_aed_param_csv(model_config[["bgc"]][["aed_zoop_pars"]],
                          file.path(model_dir, "aed_zoop_pars.csv"))
    }
    if (!is.null(model_config[["bgc"]][["aed_macrophyte_pars"]])) {
      write_aed_param_csv(model_config[["bgc"]][["aed_macrophyte_pars"]],
                          file.path(model_dir, "aed_macrophyte_pars.csv"))
    }
  }
  invisible()
}

