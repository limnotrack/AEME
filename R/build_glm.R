#' Build a glm-aed model from generic inputs
#'
#' @inheritParams build_dycd
#' @inheritParams build_aeme
#' @param lake_shape shapefile
#' @param use_lw logical, use incoming longwave radiation
#' @param overwrite_nml logical, overwrite nml file. Default is TRUE
#' @param obs_temp data.frame; observed water-column temperature profiles in the
#'   long AEME format (`Date`, `var_aeme`, `depth`, `value`), typically from
#'   [get_obs()]. When supplied, per-zone sediment-temperature
#'   parameters are derived from it via `calc_sed_temp()`; otherwise generic
#'   defaults are used. Default is `NULL`.
#' @param sed_params data.frame; `parameters(aeme)` rows for the GLM
#'   `&sediment` block (`model == "glm_aed"`, `name` like `"sediment/..."`).
#'   Keys present here are used as-is rather than estimated. Default `NULL`.
#'
#' @return Directory with GLM-AED configuration
#' @noRd
#'
#' @importFrom dplyr slice
#'

build_glm <- function(lakename, model_controls, date_range,
                      lake_shape, lat, lon, hyps,
                      lvl, inf, outf, heights_wdr, met,
                      lake_dir, config_dir, init_prof, init_depth,
                      inf_factor = 1, outf_factor = 1,
                      Kw, use_bgc, use_lw, overwrite_nml = TRUE,
                      obs_temp = NULL, sed_params = NULL) {
  
  msg <- paste0("Building GLM-AED for lake ", lakename)
  # cli_inform_safe(c("i" = msg))
  cli_safe(msg, FUN = cli::cli_h2)
  
  path_glm <- file.path(lake_dir, "glm_aed")
  
  # Create directories
  dir.create(path_glm, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(path_glm, "bcs"), showWarnings = FALSE,
             recursive = TRUE)
  dir.create(file.path(path_glm, "aed"), showWarnings = FALSE,
             recursive = TRUE)
  # Cover mass_balance bug in GLMv4
  dir.create(file.path(path_glm, "output"), showWarnings = FALSE,
             recursive = TRUE)
  
  
  # Preserve whichever GLM hydrodynamic nml version (glm3.nml, glm4.nml, ...)
  # is already present, rather than assuming glm3.nml; only fall back to
  # copying the glm3.nml template when no such file exists yet
  glm_file <- find_glm_nml(path_glm, must_exist = FALSE)
  if (is.na(glm_file)) {
    # Match the hydrodynamic nml template to the pinned/installed GLM binary
    # version (glm4.nml for GLM v4, glm3.nml for v3), falling back to glm3.nml
    # when the version can't be determined or no matching template ships.
    major <- .preferred_glm_major_version()
    nml_name <- if (!is.null(major)) sprintf("glm%d.nml", major) else "glm3.nml"
    template_file <- system.file(file.path("extdata/glm_aed", nml_name),
                                 package = "AEME")
    if (!nzchar(template_file)) {
      nml_name <- "glm3.nml"
      template_file <- system.file("extdata/glm_aed/glm3.nml", package = "AEME")
    }
    glm_file <- file.path(path_glm, nml_name)
    file.copy(template_file, glm_file)
    overwrite_nml <- TRUE
    cli_inform_safe(c("i" = "Copied in GLM nml file ({nml_name})"))
  }
  aed_file <- file.path(path_glm, "aed", "aed.nml")
  if (!file.exists(aed_file)) {
    aed_files <- list.files(system.file("extdata/aed/", package = "AEME"),
                            full.names = TRUE, pattern = "^aed[_.]")
    aed_path <- file.path(path_glm, "aed")
    dir.create(aed_path, showWarnings = FALSE)
    file.copy(aed_files, aed_path)
    cli_inform_safe(c("i" = "Copied in AED nml file and supporting files"))
  }
  plots_file <- file.path(path_glm, "plots.nml")
  if (!file.exists(plots_file)) {
    plots_file <- system.file("extdata/glm_aed/plots.nml", package = "AEME")
    file.copy(plots_file, file.path(path_glm, "plots.nml"))
    cli_inform_safe(c("i" = "Copied in GLM plots nml file"))
  }
  
  # Remove output files
  paste0(path_glm, c("/bcs", "/output")) |>
    list.files(full.names = TRUE) |>
    unlink()
  
  # Read in GLM nml file
  glm_nml <- read_nml(glm_file)
  
  # set the simulation date range
  glm_nml <- daterange_glm(date_range, glm_nml = glm_nml)
  
  
  # elipse dimensions at surface for nml
  dims_lake <- lake_dims(lake_shape)
  
  # if (nrow(hyps) > 20) {
  #   hyps <- hyps |>
  #     dplyr::slice(c(seq(1, (nrow(hyps) - 1), round(nrow(hyps) / 20)),
  #             nrow(hyps)))
  # }
  
  crest <- max(hyps[["elev"]])
  
  glm_nml <- make_stg_glm(glm_nml, lakename, bathy = hyps, lat = lat,
                         lon = lon, crest = crest, dims_lake = dims_lake,
                         use_bgc = use_bgc, obs_temp = obs_temp,
                         nml_file = basename(glm_file), sed_params = sed_params)
  
  # Make meteorology file
  make_met_glm(obs_met = met, path_glm = path_glm, use_lw = use_lw)
  # Longwave Radiation switch
  if (use_lw) {
    glm_nml$meteorology$lw_type <- "LW_IN"
  } else {
    glm_nml$meteorology$lw_type <- "LW_CC"
  }
  
  # Make inflows table and modify nml
  glm_nml <- make_inf_glm(glm_nml = glm_nml, path_glm = path_glm, list_inf = inf,
                         mass = TRUE, inf_factor = inf_factor)
  
  #--- make outflows table and modify nml
  # `heights_wdr` and GLM's `outl_elvs` are absolute elevations on the same
  # datum as the hypsography (`hyps$elev`), which for some lakes extends below
  # 0 m (e.g. a lake bed below sea level). Keep everything in that datum.
  lake_floor <- min(hyps[["elev"]])
  surface_elev <- lake_floor + init_depth
  outf[["elevation"]] <- NULL
  for (i in seq_along(heights_wdr)) {
    # A non-positive value is only a "not set" sentinel when the hypsography
    # sits at/above 0 m; when it extends below 0 m a negative elevation is a
    # legitimate absolute outlet elevation and must be kept as-is.
    if (is.na(heights_wdr[i]) || (heights_wdr[i] <= 0 && lake_floor >= 0)) {
      heights_wdr[i] <- surface_elev - 1
      next
    }
    if (heights_wdr[i] > crest || heights_wdr[i] < lake_floor) {
      cli_inform_safe(c("!" = "Withdrawal elevation is not within the range of
                        the hypsography. Setting to 0.75 of the maximum depth."))
      heights_wdr[i] <- lake_floor + (0.75 * (crest - lake_floor))
    }
  }
  outlet_type <- ifelse(heights_wdr < 0, 2, 1)
  flt_off_sw <- outlet_type == 2
  
  glm_nml <- make_wdr_glm(outf = outf,
                         heights_wdr = heights_wdr,
                         outlet_type = outlet_type,
                         flt_off_sw = flt_off_sw,
                         bathy = hyps,
                         dims_lake = dims_lake,
                         wdr_factor = outf_factor, update_nml = TRUE,
                         glm_nml = glm_nml, path_glm = path_glm)
  
  # starting water level
  glm_nml <- initialise_glm(glm_nml = glm_nml, lvl_bottom = 0.1, 
                            init_depth = init_depth, tbl_obs = init_prof,
                            Kw = Kw, model_controls = model_controls)
  
  if (use_bgc && overwrite_nml) {
    initialise_aed(model_controls = model_controls,
                   path_aed = file.path(path_glm, "aed"),
                   n_zones = glm_nml[["sediment"]][["n_zones"]])
  }

  if (use_bgc) {
    glm_nml[["wq_setup"]] <- list("wq_lib" = "aed",
                                  "wq_nml_file" = "aed/aed.nml",
                                  "ode_method" = 1,
                                  "split_factor" = 1,
                                  "bioshade_feedback" = TRUE,
                                  "repair_state" = TRUE)
    wq_nml_file <- file.path(path_glm, glm_nml[["wq_setup"]][["wq_nml_file"]])
    if (!file.exists(wq_nml_file)) {
      cli::cli_alert_warning(" {.file {wq_nml_file}} does not exist.")
    }
  } else {
    glm_nml[["wq_setup"]] <- NULL
  }

  # GLMv4: report the water/mass balance for the AED state variables that are
  # switched on (mirrors the &init_profiles wq_names initialise_glm() just
  # wrote). A glm4.nml template ships an &mass_balance block; older GLM builds
  # have none, in which case this is a no-op.
  glm_nml <- set_glm_mass_balance(glm_nml, use_bgc = use_bgc)

  # Write the GLM nml file
  if (overwrite_nml) {
    write_nml(glm_nml, glm_file)
  }
  # check_glm_nml(file = glm_file)
  
  return(invisible())
}
