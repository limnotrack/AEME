#' Set one or more parameter values in a Simstrat-AED2 `simstrat.par` file
#'
#' Thin, `aeme`-free wrapper for editing a Simstrat-AED2 `simstrat.par`
#' (JSON) file in place. Intended for a Simstrat-AED2-only workflow where a
#' user just wants to tweak parameters, run the model, and load the output.
#'
#' @param path_simstrat filepath; directory containing the Simstrat-AED2
#' configuration
#' @param ... named parameter/value pairs to set, using a dot-separated path
#' into the nested JSON structure, e.g. `` `ModelParameters.f_wind` = 1.3 ``
#' or `` `Simulation.Reference year` = 2020 ``. Values must be of the same
#' type (numeric, logical, character) as the current value in the file.
#' @param par_file filepath; path to the `simstrat.par` file to edit.
#' Defaults to `simstrat.par` in `path_simstrat`.
#'
#' @return invisibly, the updated par list
#' @export
#'
#' @examples
#' \dontrun{
#' set_simstrat_param(path_simstrat, `ModelParameters.f_wind` = 1.3)
#' }

set_simstrat_param <- function(path_simstrat, ...,
                               par_file = file.path(path_simstrat, "simstrat.par")) {

  arg_list <- list(...)
  if (length(arg_list) == 0) {
    cli::cli_abort("Provide at least one name = value pair to set.")
  }
  if (is.null(names(arg_list)) || any(names(arg_list) == "")) {
    cli::cli_abort("All arguments in '...' must be named, e.g. `ModelParameters.f_wind` = 1.3.")
  }

  par <- jsonlite::fromJSON(par_file, simplifyVector = FALSE)
  for (nm in names(arg_list)) {
    par <- .nested_list_set(par, nm, arg_list[[nm]])
  }
  jsonlite::write_json(par, par_file, pretty = TRUE, auto_unbox = TRUE,
                       null = "null")

  invisible(par)
}
