#' Write model configuration from the aeme object
#'
#' Writes each requested model's configuration files straight from the
#' `aeme` object's cached state, with no recomputation of any kind -- the
#' hydrodynamic/bgc files come verbatim from `configuration(aeme)`, and (for
#' `glm_aed`, when `include_boundary = TRUE`) the meteorology/inflow/outflow
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
#' @param include_boundary logical; also write GLM-AED's boundary-condition
#'   files (`bcs/meteo_glm.csv`, `bcs/inflow_*.csv`, `bcs/outflow_*.csv`)
#'   straight from `input(aeme)`/`inflows(aeme)`/`outflows(aeme)`. Has no
#'   effect on other models (dy_cd/gotm_wet/simstrat_aed2), which don't yet
#'   have an equivalent boundary-file writer here. Default `TRUE`.
#'
#' @return aeme object which was passed to the function,
#' @export

write_configuration <- function(aeme, model, path = getwd(),
                                include_boundary = TRUE) {

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
    gotm_wet = write_config_gotm_wet,
    simstrat_aed2 = write_config_simstrat_aed2
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

  if (include_boundary && "glm_aed" %in% model) {
    write_boundary_glm_aed(aeme = aeme,
                           model_dir = file.path(lake_dir, "glm_aed"))
  }

  return(aeme)
}

#' Write GLM-AED boundary-condition files straight from cached aeme state
#'
#' Companion to write_config_glm_aed(): writes `bcs/meteo_glm.csv`,
#' `bcs/inflow_*.csv`, and `bcs/outflow_*.csv` directly from
#' `input(aeme)`/`inflows(aeme)`/`outflows(aeme)`, with no recomputation --
#' no water balance, no hypsograph re-derivation, no unit-detection/
#' standardisation. The only transforms applied are the fixed, lossless
#' unit conversions GLM's file format itself requires (m3/day -> m3/s,
#' AED mass-unit scaling, mm -> m for rain/snow), the exact inverse of what
#' [glm_config_to_aeme()] undoes when reading these same files back in.
#'
#' @inheritParams build_aeme
#' @param model_dir character; the lake's `glm_aed` model directory.
#' @return Invisibly, `NULL`.
#' @noRd
write_boundary_glm_aed <- function(aeme, model_dir) {
  model_dir <- check_path(model_dir, create = TRUE)
  dir.create(file.path(model_dir, "bcs"), showWarnings = FALSE,
            recursive = TRUE)

  inp <- input(aeme)
  if (!is.null(inp[["meteo"]])) {
    use_lw <- if (is.null(inp[["use_lw"]])) TRUE else inp[["use_lw"]]
    make_met_glm(obs_met = inp$meteo, path_glm = model_dir, use_lw = use_lw)
  }

  inf <- inflows(aeme)[["data"]]
  if (length(inf) > 0) {
    make_inf_glm(path_glm = model_dir, list_inf = inf, update_nml = FALSE)
  }

  outf <- outflows(aeme)[["data"]]
  if (length(outf) > 0) {
    make_wdr_glm(outf = outf, path_glm = model_dir, update_nml = FALSE)
  }

  invisible()
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

