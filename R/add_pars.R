#' Add model parameters to Aeme object
#'
#' @inheritParams build_aeme
#' @param pars data frame with columns "model", "file", "name", "value", "min",
#' "max", "module" and "group.
#'
#' @returns Aeme object with parameters added
#' @export
#'

add_pars <- function(aeme, pars) {
  # Check if Aeme is of "Aeme" class
  if (!inherits(aeme, "Aeme")) {
    stop("aeme must be of class 'Aeme'")
  }

  if (!is.data.frame(pars)) {
    stop("pars must be a data frame")
  }

  pars_old <- parameters(aeme)
  if (nrow(pars_old) == 0) {
    pars_old <- data.frame()
  }
  pars_new <- dplyr::bind_rows(pars_old, pars)

  parameters(aeme) <- pars_new
  return(aeme)
}
