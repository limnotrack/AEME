#' Get current precipitation status in Aeme object
#' 
#' This function checks whether precipitation is currently set as a meteorological input
#' or as an inflow in the Aeme object. It examines the meteorological data for
#' precipitation values and the inflow data for a precipitation inflow.
#' @inheritParams build_aeme
#' @returns character. Either "precip_as_met", "precip_as_inflow" or "no_precip"
#' @export
#'

precip_status <- function(aeme) {
  aeme <- check_aeme(aeme)
  met <- get_met(aeme)
  inf <- get_inflows(aeme)
  inf_names <- names(inf)
  if (all(met[["MET_pprain"]] == 0) & all(met[["MET_ppsnow"]] == 0) &
      ("precip" %in% inf_names)) {
    return("precip_as_inflow")
  # } else if (all(met[["MET_pprain"]] == 0) & all(met[["MET_ppsnow"]] == 0) &
  #            !("precip" %in% inf_names)) {
  #   return("no_precip")
  } else if (any(met[["MET_pprain"]] > 0) | any(met[["MET_ppsnow"]] > 0) &
             !("precip" %in% inf_names)) {
    return("precip_as_met")
  }
}
