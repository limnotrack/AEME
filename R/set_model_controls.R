#' Set model controls for AEME object
#'
#' @inheritParams build_aeme
#'
#' @returns Aeme object with model controls set
#' @export

set_model_controls <- function(aeme, model_controls) {
  aeme <- check_aeme(aeme)
  if (!is.data.frame(model_controls)) {
    stop("model_controls must be a data frame.")
  }
  mod_ctrls_names <- c("var_aeme", "simulate", "inf_default", "initial_wc",
                       "initial_sed", "conversion_aed")
  if (!all(mod_ctrls_names %in% names(model_controls))) {
    stop(paste("model_controls must contain the following columns:",
               paste(mod_ctrls_names, collapse = ", ")))
  }
  config <- configuration(aeme)
  config$model_controls <- model_controls
  configuration(aeme) <- config
  return(aeme)
}
