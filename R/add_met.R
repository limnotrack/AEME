#' Add meteorological data to Aeme object
#' 
#' @inheritParams build_aeme
#' @param met data frame with meteorological data. Must include columns "Date",
#' "MET_radswd", "MET_radswd", "MET_pprain" and "MET_wndspd" or "MET_wnduvu" and
#'  "MET_wnduvv".
#'
#' @returns Aeme object with meteorological data added
#' @export
#'

add_met <- function(aeme, met) {
  inp <- aeme |> 
    check_aeme() |> 
    input()
  
  met <- check_met(met)
  inp[["meteo"]] <- met
  input(aeme) <- inp
  return(aeme)
}
