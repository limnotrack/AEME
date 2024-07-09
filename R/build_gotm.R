#' Build a GOTM-WET model from generic inputs
#'
#' @inheritParams build_aeme
#' @param nlev
#'
#' @return directory with GOTM configuration.
#' @noRd
#'

build_gotm <- function(lakename, model_controls, date_range,
                       lake_shape, gps, hyps, lake_dir,
                       lvl, inf, outf, met, init_prof, init_depth,
                       nlev = 40, ext_elev = 0,
                       outf_factor = 1.0, inf_factor = 1, Kw,
                       use_bgc, hum_type = 1, overwrite_yaml = TRUE,
                       est_swr_hr = TRUE) {

  message(paste0("Building GOTM-WET for lake ", lakename))

  path_gotm <- file.path(lake_dir, "gotm_wet")

  # Create directories
  dir.create(path_gotm, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(path_gotm, "inputs"), recursive = TRUE,
             showWarnings = FALSE)

  gotm_file <- file.path(path_gotm, "gotm.yaml")
  gotm_cfg_file <- system.file("extdata/gotm_wet/gotm.yaml",
                               package = "AEME")
  gotm_cfg_dir <- system.file("extdata/gotm_wet",
                              package = "AEME")


  if (!file.exists(gotm_file)) {
    if (!file.exists(gotm_cfg_file)) {
      stop("No '", basename(gotm_cfg_file), "' file in ", gotm_cfg_dir, "/\n")
    }
    fils <- list.files(gotm_cfg_dir, full.names = TRUE)
    file.copy(fils, file.path(path_gotm, basename(fils)))
    overwrite_yaml <- TRUE
    message("Copied all GOTM configuration files")
  }

  # housekeeping
  file.path(path_gotm, "output") |>
    list.files(full.names = T) |>
    unlink()

  gotm <- yaml::read_yaml(file.path(path_gotm, "gotm.yaml"))

  gotm <- make_yamlGOTM(gotm = gotm, lakename = lakename, date_range = date_range,
                        hyps = hyps, gps = gps, nlev = nlev, met = met, inf = inf,
                        outf = outf, init_depth = init_depth, path_gotm = path_gotm,
                        ext_elev = ext_elev, outf_factor = outf_factor,
                        inf_factor = inf_factor, Kw = Kw, use_bgc = use_bgc,
                        hum_type = hum_type, est_swr_hr = est_swr_hr)

  # Set grid
  gotm <- set_gotm_grid(gotm = gotm, depth = init_depth, path_gotm = path_gotm,
                        method = 1)

  gotm <- initialiseGOTM(gotm = gotm, lvl_bottom = 0.1, lvl_surf = lvl_start,
                         tbl_obs = init_prof,
                         tmpwtr = model_controls$initial_wc[model_controls$var_aeme == "HYD_temp"],
                         start_date = date_range[1], path_gotm = path_gotm,
                         use_bgc = use_bgc, model_controls = model_controls)

  if (overwrite_yaml) write_yaml(gotm, gotm_file)
}
