#' Remove parameter(s) from Aeme object
#'
#' @inheritParams build_aeme
#' @param name character vector with names of parameters to remove. If missing,
#' all parameters (within `model`/`file`, if supplied) are removed.
#' @param model character vector; only remove parameters for these models.
#' If missing, parameters are removed regardless of model.
#' @param file character vector; only remove parameters in these files. If
#' missing, parameters are removed regardless of file.
#'
#' @returns Aeme object with parameters removed
#' @export
#'

remove_param <- function(aeme, name, model, file) {
  # Check if aeme is a Aeme object
  aeme <- check_aeme(aeme)
  if (missing(name) && missing(model) && missing(file)) {
    parameters(aeme) <- data.frame()
  } else {
    param_old <- parameters(aeme)
    if (nrow(param_old) == 0) {
      stop("No parameters to remove")
    }
    drop <- rep(TRUE, nrow(param_old))
    if (!missing(name)) drop <- drop & param_old$name %in% name
    if (!missing(model)) drop <- drop & param_old$model %in% model
    if (!missing(file)) drop <- drop & param_old$file %in% file
    parameters(aeme) <- param_old[!drop, , drop = FALSE]
  }
  return(aeme)
}
