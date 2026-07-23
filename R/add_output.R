#' Add model output to Aeme object
#'
#' @param aeme Aeme object
#' @param out list with model output loaded with `load_output()`
#'
#' @returns Aeme object with model output added
#' @export
#'

add_output <- function(aeme, out) {
  outp <- output(aeme)
  ens_n <- outp$n_members + 1
  ens_n <- ifelse(length(ens_n) == 0, 1, ens_n)
  ens_lab <- format_ens_label(ens_n = ens_n)
  
  outp[[ens_lab]] <- list(dy_cd = out[["dy_cd"]], glm_aed = out[["glm_aed"]],
                          gotm_wet = out[["gotm_wet"]],
                          simstrat_aed2 = out[["simstrat_aed2"]])
  outp$n_members <- sum(grepl("ens", names(outp)))
  
  output(aeme) <- outp
  return(aeme)
}
