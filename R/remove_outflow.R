#' Remove outflow data from Aeme object
#' 
#' Remove specified outflow data from an Aeme object.
#'
#' @inheritParams build_aeme
#' @param outflow_id A string specifying the identifier for the outflow. 
#'
#' @returns Aeme object with outflow removed
#' 
#' @export

remove_outflow <- function(aeme, outflow_id) {
  # Check that aeme is an Aeme object
  outf <- aeme |>
    check_aeme() |>
    outflows()
  
  outf[["data"]][[outflow_id]] <- NULL
  
  outflows(aeme) <- outf
  return(aeme)
}
