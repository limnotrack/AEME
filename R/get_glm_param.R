#' Get one or more parameter values from a GLM-AED nml file
#'
#' Companion to [set_glm_param()] for reading current values without
#' needing an `aeme` object.
#'
#' @inheritParams set_glm_param
#' @param name character vector; name(s) of the nml parameter(s) to read.
#'
#' @return the parameter value if `name` has length 1, otherwise a named
#' list of values
#' @export
#'
#' @examples
#' \dontrun{
#' get_glm_param(path_glm, "Kw")
#' get_glm_param(path_glm, c("Kw", "coef_mix_hyp"))
#' }

get_glm_param <- function(path_glm, name, glm_file = find_glm_nml(path_glm)) {

  glm_nml <- read_nml(glm_file)
  vals <- lapply(name, \(n) get_nml_value(glm_nml, n))
  names(vals) <- name

  if (length(name) == 1) {
    return(vals[[1]])
  }
  vals
}
