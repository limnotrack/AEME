#' Build a glm-aed model from generic inputs
#'
#' @inheritParams build_dycd
#' @inheritParams build_aeme
#' @param lake_shape shapefile
#' @param use_lw logical, use incoming longwave radiation
#' @param overwrite_nml logical, overwrite nml file. Default is TRUE
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
                      Kw, use_bgc, use_lw, overwrite_nml = TRUE) {
  
  msg <- paste0("Building GLM-AED for lake ", lakename)
  cli_inform_safe(c("i" = msg))
  
  path_glm <- file.path(lake_dir, "glm_aed")
  
  # Create directories
  dir.create(path_glm, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(path_glm, "bcs"), showWarnings = FALSE,
             recursive = TRUE)
  dir.create(file.path(path_glm, "aed"), showWarnings = FALSE,
             recursive = TRUE)
  
  
  glm_file <- file.path(path_glm, "glm3.nml")
  if (!file.exists(glm_file)) {
    glm_file <- system.file("extdata/glm_aed/glm3.nml", package = "AEME")
    file.copy(glm_file, file.path(path_glm, "glm3.nml"))
    overwrite_nml <- TRUE
    cli_inform_safe(c("i" = "Copied in GLM nml file"))
  }
  aed_file <- file.path(path_glm, "aed", "aed.nml")
  if (!file.exists(aed_file)) {
    aed_files <- list.files(system.file("extdata/glm_aed/", package = "AEME"),
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
  glm_nml <- read_nml(file.path(path_glm, "glm3.nml"))
  
  # set the simulation date range
  glm_nml <- daterange_GLM(date_range, glm_nml = glm_nml)
  
  
  # elipse dimensions at surface for nml
  dims_lake <- lake_dims(lake_shape)
  
  # if (nrow(hyps) > 20) {
  #   hyps <- hyps |>
  #     dplyr::slice(c(seq(1, (nrow(hyps) - 1), round(nrow(hyps) / 20)),
  #             nrow(hyps)))
  # }
  
  crest <- max(hyps[["elev"]])
  
  glm_nml <- make_stgGLM(glm_nml, lakename, bathy = hyps, lat = lat,
                         lon = lon, crest = crest, dims_lake = dims_lake)
  
  # Make meteorology file
  make_metGLM(obs_met = met, path_glm = path_glm, use_lw = use_lw)
  # Longwave Radiation switch
  if (use_lw) {
    glm_nml$meteorology$lw_type <- "LW_IN"
  } else {
    glm_nml$meteorology$lw_type <- "LW_CC"
  }
  
  # Make inflows table and modify nml
  glm_nml <- make_infGLM(glm_nml = glm_nml, path_glm = path_glm, list_inf = inf,
                         mass = TRUE, inf_factor = inf_factor)
  
  #--- make outflows table and modify nml
  # heights_wdr <- max(hyps$elev) - min(hyps$elev) - 1
  outlet_type <- ifelse(heights_wdr < 0, 2, 1) 
  flt_off_sw <- outlet_type == 2
  outf[["elevation"]] <- NULL
  for (i in seq_along(heights_wdr)) {
    if (is.na(heights_wdr[i]) | heights_wdr[i] <= 0) {
      heights_wdr[i] <- init_depth - 1
      next
    }
    if (heights_wdr[i] > max(hyps$elev) || heights_wdr[i] < min(hyps$elev)) {
      cli_inform_safe(c("!" = "Withdrawal depth is not within the range of the 
                        hypsography. Setting to 0.75 of the maximum depth."))
      heights_wdr[i] <- min(hyps$elev) + (0.75 * (max(hyps$elev) - min(hyps$elev)))
    }
  }
  
  glm_nml <- make_wdrGLM(outf = outf,
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
  
  if (use_bgc) {
    initialise_aed(model_controls = model_controls,
                   path_aed = file.path(path_glm, "aed"))
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
  
  # Write the GLM nml file
  if (overwrite_nml) {
    write_nml(glm_nml, file.path(path_glm, "glm3.nml"))
  }
  # check_glm_nml(file = file.path(path_glm, "glm3.nml"))
  
  return(invisible())
}
