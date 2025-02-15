#' Remove output from AEME object
#'
#' @inheritParams build_aeme
#'
#' @inherit build_aeme return
#' @export
#'

remove_output <- function(aeme) {
  outp <- output(aeme)
  outp <- list(n_members = 0)
  output(aeme) <- outp
  return(aeme)
}
