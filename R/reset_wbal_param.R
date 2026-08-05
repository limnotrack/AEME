#' Reset water balance parameters
#' 
#' This function resets the water balance parameters in the Aeme object. This is
#' useful if you want to start fresh with a new set of parameters for example 
#' if you add/remove a inflow or change the meteorological data.
#'
#' @inheritParams build_aeme 
#'
#' @returns Aeme object with water balance parameters reset
#' @export
#'

reset_wbal_param <- function(aeme) {
  aeme <- check_aeme(aeme)
  wbal <- aeme |> 
    water_balance()
  
  wbal[["params"]] <- NULL
  
  water_balance(aeme) <- wbal
  return(aeme)
}
