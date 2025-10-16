#' Add model parameters to Aeme object
#'
#' @inheritParams build_aeme
#' @param param data frame with columns "model", "file", "name", "value", "min",
#' "max", "module" and "group.
#'
#' @returns Aeme object with parameters added
#' @export
#'

add_param <- function(aeme, param) {
  # Check if aeme is a Aeme object
  aeme <- check_aeme(aeme)
  
  if (!is.data.frame(param)) {
    stop("param must be a data frame")
  }

  param_old <- parameters(aeme)
  if (nrow(param_old) == 0) {
    param_old <- data.frame()
  } else {
    # If duplicates remove them
    param_old <- dplyr::anti_join(param_old, param,
                                  by = c("model", "file", "name"))
  }
  param_new <- dplyr::bind_rows(param_old, param)

  parameters(aeme) <- param_new
  return(aeme)
}
