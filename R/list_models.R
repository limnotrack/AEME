#' Get a vector of available models
#'
#' @inheritParams build_aeme
#'
#' @returns vector of available models
#' @export
#'

list_models <- function(aeme = NULL) {
  models <- c("DYRESM-CAEDYM" = "dy_cd", "GLM-AED" = "glm_aed",
              "GOTM-WET" = "gotm_wet")

  if (!is.null(aeme)) {
    cfg <- configuration(aeme)
    chk <- sapply(models, \(m) !is.null(cfg[[m]][["hydrodynamic"]]))
    models <- models[chk]
  }
  return(models)
}
