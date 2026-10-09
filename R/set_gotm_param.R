#' Set one or more parameter values in a GOTM-WET `gotm.yaml` file
#'
#' Thin, `aeme`-free wrapper for editing a GOTM-WET `gotm.yaml` file in
#' place. Intended for a GOTM-WET-only workflow where a user just wants to
#' tweak parameters, run the model, and load the output.
#'
#' @param path_gotm filepath; directory containing the GOTM-WET
#' configuration
#' @param ... named parameter/value pairs to set, using a dot-separated path
#' into the nested yaml structure, e.g. `` `time.dt` = 1800 `` or
#' `` `location.latitude` = -36.9 ``. Values must be of the same type
#' (numeric, logical, character) as the current value in the file.
#' @param yaml_file filepath; path to the `gotm.yaml` file to edit. Defaults
#' to `gotm.yaml` in `path_gotm`.
#'
#' @return invisibly, the updated yaml list
#' @export
#'
#' @examples
#' \dontrun{
#' set_gotm_param(path_gotm, `time.dt` = 1800)
#' }

set_gotm_param <- function(path_gotm, ...,
                           yaml_file = file.path(path_gotm, "gotm.yaml")) {

  arg_list <- list(...)
  if (length(arg_list) == 0) {
    cli::cli_abort("Provide at least one name = value pair to set.")
  }
  if (is.null(names(arg_list)) || any(names(arg_list) == "")) {
    cli::cli_abort("All arguments in '...' must be named, e.g. `time.dt` = 1800.")
  }

  gotm <- yaml::read_yaml(yaml_file)
  for (nm in names(arg_list)) {
    gotm <- .nested_list_set(gotm, nm, arg_list[[nm]])
  }
  write_yaml(gotm, yaml_file)

  invisible(gotm)
}
