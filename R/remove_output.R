#' Remove output from AEME object
#'
#' @inheritParams build_aeme
#'
#' @inherit build_aeme return
#' @export
#'

remove_output <- function(aeme) {
  output(aeme) <- .empty_output()
  return(aeme)
}
