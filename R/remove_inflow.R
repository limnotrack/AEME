#' Remove inflow data from Aeme object
#' 
#' Remove inflow data from an Aeme object. You can specify a particular inflow 
#' to remove by providing its identifier, or you can remove all inflows at once. 
#' If you choose to remove all inflows, the inflow_id argument will be ignored.
#'
#' @inheritParams build_aeme
#' @param inflow_id A string specifying the identifier for the inflow. 
#' @param all A logical value indicating whether to remove all inflows. If TRUE,
#'  the inflow_id argument is ignored and all inflows are removed.
#'
#' @returns Aeme object with inflow removed
#' 
#' @export

remove_inflow <- function(aeme, inflow_id = NULL, all = FALSE) {
  # Check that aeme is an Aeme object
  inf <- aeme |>
    check_aeme() |>
    inflows()
  
  if (all && !is.null(inflow_id)) {
    cli::cli_abort("Provide either inflow_id or all = TRUE, not both.")
  }
  
  
  if (all) {
    precip_status <- precip_status(aeme)
    if (precip_status == "precip_as_inflow") {
      aeme <- set_precip(aeme, type = "precip_as_met")
    }
    inf[["data"]] <- list()
    
  } else {
    
    if (is.null(inflow_id)) {
      cli::cli_abort("Must provide inflow_id unless all = TRUE")
    }
    
    if (!inflow_id %in% names(inf[["data"]])) {
      # stop(sprintf("Inflow '%s' not found", inflow_id))
      cli::cli_abort(c(
        "Inflow '{inflow_id}' not found.",
        "i" = "Available inflows: {names(inf[['data']])}"
      ))
    }
    
    inf[["data"]][[inflow_id]] <- NULL
  }
  
  inflows(aeme) <- inf
  return(aeme)
}
