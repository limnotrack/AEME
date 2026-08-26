#' Set inflow data for a Simstrat-AED2 simulation directory
#'
#' Thin, `aeme`-free wrapper around the internal inflow writer used by
#' [build_aeme()]. Writes `Qinp.dat`/`Tinp.dat`/`Sinp.dat` (and, if BGC is
#' coupled, `AED2_inflow/*.dat`) into `path_simstrat`.
#'
#' @param path_simstrat filepath; to Simstrat-AED2 directory
#' @param inf named list of data.frames, one per inflow. Each must have a
#' `Date` column plus `HYD_flow`, `HYD_temp`, `CHM_salt` columns -- see
#' [add_inflow()] for the expected schema. All inflows are summed into a
#' single combined series (Simstrat only accepts one inflow series), matching
#' `make_inf_simstrat()`'s existing behaviour.
#' @param inf_factor numeric; scaling factor applied to all inflow flow
#' rates. Default is `1`.
#' @param use_bgc logical; also write AED2 inflow concentration files.
#' Defaults to the existing `ModelConfig.CoupleAED2` setting in
#' `simstrat.par`.
#' @param model_controls data.frame of loaded model controls (see
#' [get_model_controls()]), required when `use_bgc = TRUE`.
#' @param ref_year integer; Simstrat's `Simulation.Reference year`. Defaults
#' to the value already in `simstrat.par`.
#' @param par_file filepath; path to the `simstrat.par` file to read
#' `use_bgc`/`ref_year` defaults from. Defaults to `simstrat.par` in
#' `path_simstrat`.
#'
#' @return invisibly, `NULL`
#' @export
#'
#' @examples
#' \dontrun{
#' set_simstrat_inflows(path_simstrat, inf = list(stream1 = inflow_df))
#' }

set_simstrat_inflows <- function(path_simstrat, inf, inf_factor = 1,
                                 use_bgc = NULL, model_controls = NULL,
                                 ref_year = NULL,
                                 par_file = file.path(path_simstrat, "simstrat.par")) {

  if (!is.list(inf) || is.null(names(inf)) || any(names(inf) == "")) {
    cli::cli_abort("'inf' must be a named list of data.frames.")
  }

  par <- jsonlite::fromJSON(par_file, simplifyVector = FALSE)
  is_aed <- "AEDConfig" %in% names(par)
  bgc_cfg_key <- if (is_aed) "AEDConfig" else "AED2Config"
  bgc_tag <- if (is_aed) "AED" else "AED2"
  if (is.null(use_bgc)) {
    use_bgc <- isTRUE(par[["ModelConfig"]][[paste0("Couple", bgc_tag)]])
  }
  if (isTRUE(use_bgc) && is.null(model_controls)) {
    cli::cli_abort("'model_controls' is required when 'use_bgc = TRUE' -- see get_model_controls().")
  }
  if (is.null(ref_year)) {
    ref_year <- as.integer(par[["Simulation"]][["Reference year"]])
  }

  # BGC files may live in a subdirectory of path_simstrat (see
  # build_simstrat()) -- inferred from the configured BGC nml file's own
  # directory, so this works whether or not that subdirectory is used.
  config_file <- par[[bgc_cfg_key]][[paste0(bgc_tag, "ConfigFile")]]
  bgc_dir <- if (!is.null(config_file)) {
    file.path(path_simstrat, dirname(config_file))
  } else {
    path_simstrat
  }

  # surface_elev only matters to make_inf_simstrat() for depth-referenced
  # inflows, which it does not currently implement (a fixed value here has
  # no effect on the written files)
  make_inf_simstrat(inf = inf, path_simstrat = path_simstrat, bgc_dir = bgc_dir,
                    surface_elev = 0,
                    inf_factor = inf_factor, model_controls = model_controls,
                    use_bgc = use_bgc, ref_year = ref_year,
                    model = if (is_aed) "simstrat_aed" else "simstrat_aed2")

  invisible(NULL)
}
