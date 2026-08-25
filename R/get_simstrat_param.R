#' Get one or more parameter values from a Simstrat-AED2 `simstrat.par` file
#'
#' Companion to [set_simstrat_param()] for reading current values without
#' needing an `aeme` object.
#'
#' @inheritParams set_simstrat_param
#' @param name character vector; dot-separated path(s) to read, e.g.
#' `"ModelParameters.f_wind"` or `` c("Simulation.Reference year", "ModelParameters.lat") ``.
#'
#' @return the parameter value if `name` has length 1, otherwise a named
#' list of values
#' @export
#'
#' @examples
#' \dontrun{
#' get_simstrat_param(path_simstrat, "ModelParameters.f_wind")
#' get_simstrat_param(path_simstrat, c("ModelParameters.f_wind", "ModelParameters.lat"))
#' }

get_simstrat_param <- function(path_simstrat, name,
                               par_file = file.path(path_simstrat, "simstrat.par")) {

  par <- jsonlite::fromJSON(par_file, simplifyVector = FALSE)
  vals <- lapply(name, \(nm) .nested_list_get(par, nm))
  names(vals) <- name

  if (length(name) == 1) {
    return(vals[[1]])
  }
  vals
}
