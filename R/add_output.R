#' Add model output to Aeme object
#'
#' Stores `out` as the next ensemble member of the `output` slot.
#'
#' @param aeme Aeme object
#' @param out list with model output loaded with `load_output()`
#'
#' @returns Aeme object with model output added
#' @export
#'

add_output <- function(aeme, out) {
  outp <- output(aeme)
  ens_n <- .count_members(outp) + 1L
  output(aeme) <- .set_member(outp, ens_n = ens_n, mods = out)
  return(aeme)
}
