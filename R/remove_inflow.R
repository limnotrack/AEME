#' Remove inflow data from Aeme object
#' 
#' Remove specified inflow data from an Aeme object.
#'
#' @inheritParams build_aeme
#' @param inflow_id A string specifying the identifier for the inflow. 
#'
#' @returns Aeme object with inflow removed
#' 
#' @export

remove_inflow <- function(aeme, inflow_id) {
  # Check that aeme is an Aeme object
  inf <- aeme |>
    check_aeme() |>
    inflows()
  
  inf[["data"]][[inflow_id]] <- NULL
  
  inflows(aeme) <- inf
  return(aeme)
}
