#' Get a vector of available models
#'
#' @inheritParams build_aeme
#'
#' @returns vector of available models
#' @export
#'

list_models <- function(aeme = NULL) {
  all_models <- c("DYRESM-CAEDYM" = "dy_cd", "GLM-AED" = "glm_aed",
                  "GOTM-WET" = "gotm_wet", "SIMSTRAT-AED2" = "simstrat_aed2",
                  "SIMSTRAT-AED" = "simstrat_aed")

  if (!is.null(aeme)) {
    cfg <- configuration(aeme)
    models <- cfg[["model"]]
    if (is.null(models)) {
      chk <- sapply(all_models, \(m) !is.null(cfg[[m]][["hydrodynamic"]]))
      models <- all_models[chk]
    }
  } else {
    models <- all_models
  }
  return(models)
}
