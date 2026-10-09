#' Set inflow data for a GLM-AED simulation directory
#'
#' Thin, `aeme`-free wrapper around the internal inflow writer used by
#' [build_aeme()]. Writes one boundary-condition csv per inflow into
#' `path_glm/bcs` and updates the `&inflow` block of the GLM nml file to
#' point at them.
#'
#' @param path_glm filepath; to GLM-AED directory (containing the nml file
#' and a `bcs/` subdirectory)
#' @param list_inf named list of data.frames, one per inflow. Each must have
#' a `Date` column plus flow/temperature/salt columns (e.g. `HYD_flow`,
#' `HYD_temp`, `CHM_salt`) -- see [add_inflow()] for the expected schema.
#' @param inf_factor numeric; scaling factor applied to all inflow flow
#' rates. Default is `1`.
#' @param mass logical; convert inflow variables to GLM-AED mass units using
#' the package's built-in conversion table. Default is `TRUE`.
#' @param glm_file filepath; path to the GLM hydrodynamic nml file to
#' update. Defaults to the file found in `path_glm` via [find_glm_nml()].
#'
#' @return invisibly, the updated nml object
#' @export
#'
#' @examples
#' \dontrun{
#' set_glm_inflows(path_glm, list_inf = list(stream1 = inflow_df))
#' }

set_glm_inflows <- function(path_glm, list_inf, inf_factor = 1, mass = TRUE,
                            glm_file = find_glm_nml(path_glm)) {

  if (!is.list(list_inf) || is.null(names(list_inf)) ||
      any(names(list_inf) == "")) {
    cli::cli_abort("'list_inf' must be a named list of data.frames.")
  }

  glm_nml <- read_nml(glm_file)
  glm_nml <- make_inf_glm(glm_nml = glm_nml, path_glm = path_glm,
                          list_inf = list_inf, mass = mass,
                          inf_factor = inf_factor, update_nml = TRUE)
  write_nml(glm_nml, file = glm_file)

  invisible(glm_nml)
}
