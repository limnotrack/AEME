#' Get one or more parameter values from a GOTM-WET `gotm.yaml` file
#'
#' Companion to [set_gotm_param()] for reading current values without
#' needing an `aeme` object.
#'
#' @inheritParams set_gotm_param
#' @param name character vector; dot-separated path(s) to read, e.g.
#' `"time.dt"` or `c("time.dt", "location.latitude")`.
#'
#' @return the parameter value if `name` has length 1, otherwise a named
#' list of values
#' @export
#'
#' @examples
#' \dontrun{
#' get_gotm_param(path_gotm, "time.dt")
#' get_gotm_param(path_gotm, c("time.dt", "location.latitude"))
#' }

get_gotm_param <- function(path_gotm, name,
                           yaml_file = file.path(path_gotm, "gotm.yaml")) {

  gotm <- yaml::read_yaml(yaml_file)
  vals <- lapply(name, \(nm) .nested_list_get(gotm, nm))
  names(vals) <- name

  if (length(name) == 1) {
    return(vals[[1]])
  }
  vals
}
