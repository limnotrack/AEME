#' Remove outflow data from Aeme object
#' 
#' Remove specified outflow data from an Aeme object.
#'
#' @inheritParams build_aeme
#' @inheritParams remove_inflow
#' @param outflow_id A string specifying the identifier for the outflow. 
#'
#' @returns Aeme object with outflow removed
#' 
#' @export

remove_outflow <- function(aeme, outflow_id = NULL, all = FALSE) {
  # Check that aeme is an Aeme object
  check_aeme(aeme)
  outf <- aeme |>
    outflows()
  
  if (all && !is.null(outflow_id)) {
    cli::cli_abort("Provide either outflow_id or all = TRUE, not both.")
  }
  
  if (all) {
    outf[["data"]] <- list()
  } else {
    if (is.null(outflow_id)) {
      cli::cli_abort("Must provide outflow_id unless all = TRUE")
    }
    
    if (!outflow_id %in% names(inf[["data"]])) {
      # stop(sprintf("Inflow '%s' not found", outflow_id))
      cli::cli_abort(c(
        "Outflow '{outflow_id}' not found.",
        "i" = "Available inflows: {names(inf[['data']])}"
      ))
    }
    
    outf[["data"]][[outflow_id]] <- NULL
  }
  
  outflows(aeme) <- outf
  return(aeme)
}
