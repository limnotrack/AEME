#' Remove parameter(s) from Aeme object
#' 
#' @inheritParams build_aeme
#' @param name character vector with names of parameters to remove. If missing,
#' all parameters are removed.
#'
#' @returns Aeme object with parameters removed
#' @export
#'

remove_param <- function(aeme, name) {
  # Check if aeme is a Aeme object
  aeme <- check_aeme(aeme)
  if (missing(name)) {
    parameters(aeme) <- data.frame()
  } else {
    param_old <- parameters(aeme)
    if (nrow(param_old) == 0) {
      stop("No parameters to remove")
    }
    param_new <- param_old[!param_old$name %in% name, ]
    parameters(aeme) <- param_new
  }
  return(aeme)
}
