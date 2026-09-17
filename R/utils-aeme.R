#' Set the model for an AEME object
#' @noRd
set_model <- function(aeme, model) {
  cfg <- configuration(aeme)
  cfg[["model"]] <- model
  configuration(aeme) <- cfg
  return(aeme)
}
