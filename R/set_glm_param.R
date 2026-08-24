#' Set one or more parameter values in a GLM-AED nml file
#'
#' Thin wrapper around [read_nml()]/[set_nml()]/[write_nml()] for editing a
#' single GLM-AED `.nml` file in place, without needing an `aeme` object.
#' Intended for a GLM-AED-only workflow where a user just wants to tweak
#' parameters, run the model, and load the output.
#'
#' @param path_glm filepath; directory containing the GLM-AED configuration
#' @param ... named parameter/value pairs to set, e.g. `Kw = 0.5`,
#' `coef_mix_hyp = 0.3`. Values must be of the same type (numeric, logical,
#' character) as the current value in the nml file.
#' @param glm_file filepath; path to the nml file to edit. Defaults to the
#' GLM hydrodynamic nml (`glm3.nml`/`glm4.nml`) found in `path_glm` via
#' [find_glm_nml()]. Pass the `aed2.nml` path directly to edit AED
#' parameters instead.
#'
#' @return invisibly, the updated nml object
#' @export
#'
#' @examples
#' \dontrun{
#' set_glm_param(path_glm, Kw = 0.5, coef_mix_hyp = 0.3)
#' set_glm_param(path_glm, glm_file = file.path(path_glm, "aed", "aed2.nml"),
#'               p_max = 1.2)
#' }

set_glm_param <- function(path_glm, ..., glm_file = find_glm_nml(path_glm)) {

  arg_list <- list(...)
  if (length(arg_list) == 0) {
    cli::cli_abort("Provide at least one name = value pair to set.")
  }
  if (is.null(names(arg_list)) || any(names(arg_list) == "")) {
    cli::cli_abort("All arguments in '...' must be named, e.g. Kw = 0.5.")
  }

  glm_nml <- read_nml(glm_file)
  glm_nml <- set_nml(glm_nml, arg_list = arg_list)
  write_nml(glm_nml, file = glm_file)

  invisible(glm_nml)
}
