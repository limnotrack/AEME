#' Build a GOTM-WET model from generic inputs
#'
#' @inheritParams build_aeme
#' @param nlev number of vertical levels in GOTM. Default is 40.
#'
#' @return directory with GOTM configuration.
#' @noRd
#'

build_gotm <- function(lakename, model_controls, date_range,
                       lake_shape, lat, lon, hyps, lake_dir,
                       lvl, inf, outf, met, init_prof, init_depth,
                       nlev = 40, outf_factor = 1.0, inf_factor = 1, Kw,
                       use_bgc, hum_type = 1, overwrite_yaml = TRUE,
                       est_swr_hr = TRUE, time_step = 3600,
                       output_time_step = 86400, output_daily_mean = FALSE) {

  msg <- paste0("Building GOTM-WET model for lake ", lakename)
  cli_inform_safe(c("i" = msg))

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
    cli_inform_safe(c("i" = "Copied in GOTM configuration files"))
  }

  # housekeeping
  file.path(path_gotm, "output") |>
    list.files(full.names = T) |>
    unlink()

  gotm <- yaml::read_yaml(file.path(path_gotm, "gotm.yaml"))

  gotm <- make_yaml_gotm(gotm = gotm, lakename = lakename, date_range = date_range,
                        hyps = hyps, lat = lat, lon = lon, nlev = nlev,
                        met = met, inf = inf, outf = outf,
                        init_depth = init_depth, path_gotm = path_gotm,
                        outf_factor = outf_factor,
                        inf_factor = inf_factor, Kw = Kw, use_bgc = use_bgc,
                        hum_type = hum_type, est_swr_hr = est_swr_hr,
                        time_step = time_step)

  # Output cadence. The shipped output.yaml is daily (output\output:
  # hour/24); only rewrite it for sub-daily output so the daily path is
  # byte-for-byte unchanged. AEME never disaggregates forcing.
  out_yaml_file <- file.path(path_gotm, "output.yaml")
  if (isTRUE(output_time_step < 86400) && file.exists(out_yaml_file)) {
    out_yaml <- yaml::read_yaml(out_yaml_file)
    main_key <- grep("output.output$", names(out_yaml), value = TRUE)
    if (length(main_key) == 1) {
      out_yaml[[main_key]][["time_unit"]] <- "second"
      out_yaml[[main_key]][["time_step"]] <- as.integer(output_time_step)
      write_yaml(out_yaml, out_yaml_file)
    }
  }

  # Daily-mean stream (time(aeme)$output_daily_mean). GOTM already ships an
  # `output_daily` block with `time_method: mean`; when the option is on,
  # restrict it to the targeted variables (the model_controls `simulate` set
  # plus the fixed set GOTM's reader needs) instead of writing every variable.
  if (isTRUE(output_daily_mean) && file.exists(out_yaml_file)) {
    if (!missing(met) && !is.null(met) && !is_subdaily(met[["Date"]])) {
      cli_inform_safe(c(
        "!" = paste("GOTM daily-mean output requested but the meteorology is",
                    "daily; the daily mean will equal the daily value.")
      ))
    }
    out_yaml <- yaml::read_yaml(out_yaml_file)
    dkey <- grep("output.output_daily$", names(out_yaml), value = TRUE)
    if (length(dkey) == 1) {
      keep_vars <- tryCatch(
        suppressWarnings(.map_output_vars(
          get_vars_sim(model_controls = model_controls), "gotm_wet")),
        error = function(e) NULL)
      out_yaml[[dkey]][["format"]] <- "netcdf"
      out_yaml[[dkey]][["time_unit"]] <- "day"
      out_yaml[[dkey]][["time_step"]] <- 1L
      out_yaml[[dkey]][["time_method"]] <- "mean"
      if (length(keep_vars)) {
        out_yaml[[dkey]][["variables"]] <-
          lapply(keep_vars, function(v) list(source = v))
      }
      write_yaml(out_yaml, out_yaml_file)
    }
  }

  # Set grid
  gotm <- set_gotm_grid(gotm = gotm, depth = init_depth, path_gotm = path_gotm,
                        method = 1)

  gotm <- initialise_gotm(gotm = gotm, lvl_bottom = 0.1, lvl_surf = lvl_start,
                         tbl_obs = init_prof,
                         tmpwtr = model_controls$initial_wc[model_controls$var_aeme == "HYD_temp"],
                         start_date = date_range[1], path_gotm = path_gotm,
                         use_bgc = use_bgc, model_controls = model_controls)

  if (overwrite_yaml) write_yaml(gotm, gotm_file)
  return(gotm_file)
  # check_gotm_yaml(file = gotm_file)
}
