#' Write a model's boundary-condition files
#'
#' Writes the meteorology, outflow and inflow files for one model in that
#' model's own format, from data passed in. Nothing is recomputed: the data
#' comes from `input(aeme)`, `outflows(aeme)` and `inflows(aeme)` (optionally
#' scaled by `apply_boundary_params()`), and only the fixed conversions a
#' model's file format needs are applied. Used by both
#' [input_model_parameters()] and [write_configuration()].
#'
#' @param aeme Aeme object, for the lake, hypsograph and stored configuration.
#' @param model character; one model.
#' @param lake_dir character; the lake directory containing the model
#'   directory.
#' @param meteo data.frame or `NULL`; meteorology to write. `NULL` writes none.
#' @param outf list or `NULL`; outflow data (`outflows(aeme)$data`) to
#'   write. `NULL` writes none.
#' @param inf list or `NULL`; inflow data (`inflows(aeme)$data`) to write.
#'   `NULL` writes none.
#' @param inf_factor numeric; factor(s) to scale the inflows by.
#'
#' @return Invisibly, `NULL`.
#' @noRd
write_boundary_data <- function(aeme, model, lake_dir, meteo = NULL,
                                outf = NULL, inf = NULL,
                                inf_factor = 1) {
  m <- model
  model_path <- check_path(file.path(lake_dir, m), create = TRUE)
  lke <- lake(aeme)
  lakename <- tolower(lke[["name"]])
  inp <- input(aeme)
  cfg <- configuration(aeme)
  use_lw <- if (is.null(inp[["use_lw"]])) TRUE else inp[["use_lw"]]
  hyps_elev <- inp[["hypsograph"]][["elev"]]
  simstrat <- m %in% c("simstrat_aed2", "simstrat_aed")

  ref_year <- function() {
    par <- jsonlite::fromJSON(file.path(model_path, "simstrat.par"),
                              simplifyVector = FALSE)
    as.integer(par$Simulation$`Reference year`)
  }

  # Meteorology ----
  if (!is.null(meteo)) {
    switch(
      m,
      glm_aed = {
        dir.create(file.path(model_path, "bcs"), showWarnings = FALSE,
                   recursive = TRUE)
        make_met_glm(obs_met = meteo, path_glm = model_path, use_lw = use_lw)
      },
      gotm_wet = {
        dir.create(file.path(model_path, "inputs"), showWarnings = FALSE,
                   recursive = TRUE)
        make_met_gotm(df_met = meteo, path.gotm = model_path,
                      hum_type = if (is.null(cfg[["hum_type"]])) 3 else
                        cfg[["hum_type"]],
                      est_swr_hr = if (is.null(cfg[["est_swr_hr"]])) TRUE else
                        cfg[["est_swr_hr"]],
                      return_colname = FALSE, lat = lke$latitude,
                      lon = lke$longitude)
      },
      simstrat_aed2 = ,
      simstrat_aed = {
        make_met_simstrat(met = meteo, path_simstrat = model_path,
                          ref_year = ref_year())
      },
      dy_cd = {
        z_max <- max(hyps_elev) - min(hyps_elev)
        make_dy_met(lakename = lakename, info = "test", obsMet = meteo,
                    filePath = model_path, infRain = FALSE, wndType = 0,
                    metHeight = 15, z_max = z_max, use_lw = use_lw)
      }
    )
  }

  # Outflows ----
  # Simstrat needs its outflow file even when there are no outflows
  if (!is.null(outf) && (length(outf) > 0 || simstrat)) {
    switch(
      m,
      glm_aed = {
        dir.create(file.path(model_path, "bcs"), showWarnings = FALSE,
                   recursive = TRUE)
        make_wdr_glm(outf = outf, path_glm = model_path,
                     update_nml = FALSE)
      },
      gotm_wet = {
        dir.create(file.path(model_path, "inputs"), showWarnings = FALSE,
                   recursive = TRUE)
        make_wdr_gotm(outf = outf, path_gotm = model_path, outf_factor = 1)
      },
      simstrat_aed2 = ,
      simstrat_aed = {
        surface_elev <- min(hyps_elev) + inp[["init_depth"]]
        make_wdr_simstrat(outf = outf,
                          heights_wdr = unlist(outflows(aeme)[["elevation"]]),
                          path_simstrat = model_path,
                          surface_elev = surface_elev, outf_factor = 1,
                          ref_year = ref_year(), model = m)
      },
      dy_cd = {
        make_dy_wdr(lakename = lakename, wdrData = outf,
                    filePath = model_path, info = "test")
      }
    )
  }

  # Inflows ----
  if (!is.null(inf) && (length(inf) > 0 || simstrat)) {
    switch(
      m,
      glm_aed = {
        dir.create(file.path(model_path, "bcs"), showWarnings = FALSE,
                   recursive = TRUE)
        make_inf_glm(path_glm = model_path, list_inf = inf,
                     update_nml = FALSE, inf_factor = inf_factor)
      },
      gotm_wet = {
        dir.create(file.path(model_path, "inputs"), showWarnings = FALSE,
                   recursive = TRUE)
        use_bgc <- bgc_active(cfg, m)
        make_inf_gotm(inf_list = inf, inf_factor = inf_factor,
                      use_bgc = use_bgc, path_gotm = model_path,
                      update_gotm = FALSE)
      },
      simstrat_aed2 = ,
      simstrat_aed = {
        use_bgc <- bgc_active(cfg, m)
        surface_elev <- min(hyps_elev) + inp[["init_depth"]]
        # BGC files live in a subdirectory of model_path (e.g. "aed2"/"aed")
        # -- see build_simstrat()
        bgc_dir <- file.path(model_path, sub("^simstrat_", "", m))
        inflow_dir <- if (m == "simstrat_aed") "AED_inflow" else "AED2_inflow"
        dir.create(file.path(bgc_dir, inflow_dir), showWarnings = FALSE,
                   recursive = TRUE)
        make_inf_simstrat(inf = inf, path_simstrat = model_path,
                          bgc_dir = bgc_dir, surface_elev = surface_elev,
                          inf_factor = inf_factor,
                          model_controls = cfg$model_controls,
                          use_bgc = use_bgc, ref_year = ref_year(), model = m)
      },
      dy_cd = {
        make_dy_inf(lakename = lakename, infList = inf,
                    filePath = model_path, inf_factor = inf_factor)
      }
    )
  }

  invisible()
}

#' Was a model built with its biogeochemistry switched on?
#'
#' Uses the `use_bgc` flag stored in the configuration when there is one. Its
#' absence (e.g. a configuration loaded from files) falls back to whether the
#' model has any bgc configuration: the bgc template files are present even
#' for models built without bgc, so this is only a best guess.
#'
#' @param cfg list; `configuration(aeme)`.
#' @param model character; one model.
#' @noRd
bgc_active <- function(cfg, model) {
  if (!is.null(cfg[["use_bgc"]])) return(isTRUE(cfg[["use_bgc"]]))
  !is.null(cfg[[model]][["bgc"]])
}
