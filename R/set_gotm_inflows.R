#' Set inflow data for a GOTM-WET simulation directory
#'
#' Thin, `aeme`-free wrapper around the internal inflow writer used by
#' [build_aeme()]. Writes `inputs/inf_{flow,temp,salt}_<name>.dat` (and, if
#' BGC is coupled, `inputs/inf_chem_<name>.dat`) per inflow into
#' `path_gotm`, and updates the `streams` block of `gotm.yaml` to point at
#' them. Existing stream entries not named in `inf_list` are left untouched.
#'
#' @param path_gotm filepath; to GOTM-WET directory (containing `gotm.yaml`
#' and an `inputs/` subdirectory)
#' @param inf_list named list of data.frames, one per inflow. Each must have
#' a `Date` column plus flow/temperature/salt columns (e.g. `HYD_flow`,
#' `HYD_temp`, `CHM_salt`) -- see [add_inflow()] for the expected schema.
#' @param inf_factor numeric; scaling factor applied to all inflow flow
#' rates. Default is `1`.
#' @param use_bgc logical; also write BGC concentration inflow files.
#' Defaults to the existing `fabm.use` setting in `gotm.yaml`.
#' @param yaml_file filepath; path to the `gotm.yaml` file to update.
#' Defaults to `gotm.yaml` in `path_gotm`.
#'
#' @return invisibly, the updated yaml list
#' @export
#'
#' @examples
#' \dontrun{
#' set_gotm_inflows(path_gotm, inf_list = list(stream1 = inflow_df))
#' }

set_gotm_inflows <- function(path_gotm, inf_list, inf_factor = 1,
                             use_bgc = NULL,
                             yaml_file = file.path(path_gotm, "gotm.yaml")) {

  if (!is.list(inf_list) || is.null(names(inf_list)) ||
      any(names(inf_list) == "")) {
    cli::cli_abort("'inf_list' must be a named list of data.frames.")
  }

  gotm <- yaml::read_yaml(yaml_file)
  if (is.null(use_bgc)) {
    use_bgc <- isTRUE(gotm[["fabm"]][["use"]])
  }

  gotm <- make_inf_gotm(inf_list = inf_list, inf_factor = inf_factor,
                        path_gotm = path_gotm, gotm = gotm,
                        update_gotm = TRUE, use_bgc = use_bgc)
  write_yaml(gotm, yaml_file)

  invisible(gotm)
}
