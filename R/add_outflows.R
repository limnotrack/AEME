#' Add outflows to Aeme object
#'
#' @inheritParams build_aeme
#' @param data named list with data frames for each outflow. Each data frame 
#' must have columns "Date", "outflow". If NULL, no outflows are added.
#' @param elevation named list with elevation of each outflow. If NULL, no
#' outflows are added. If elevation is -1, the outflow is assumed to
#' be at the same elevation as the hypsograph. If elevation is not -1, it must be
#' within the range of the hypsograph elevation.
#' @param factor named list with scaling factors to apply to outflows per model.
#' If NULL, no scaling is applied. 
#'
#' @returns Aeme object with outflows added
#' @export
#'

add_outflows <- function(aeme, data = NULL, elevation = NULL, factor = NULL) {
  # Check if aeme is a Aeme object
  aeme <- check_aeme(aeme)
  outf <- outflows(aeme)
  if (!is.null(data) && !is.list(data)) {
    cli::cli_abort("data must be a named list or NULL")
  }
  if (is.null(data)) {
    data <- outf$data
  }
  if (is.null(elevation)) {
    elevation <- outf$elevation
  }
  if (is.null(factor)) {
    factor <- outf$factor
  }

  if (!is.list(elevation)) {
    cli::cli_abort("elevation must be a named list")
  }
  
  # Check if all data names are in elevation list
  if (!all(names(data) %in% names(elevation))) {
    missing_names <- setdiff(names(data), names(elevation))
    cli::cli_abort("The following names in data are missing in elevation: 
                   {missing_names}")
  }
  
  inp <- input(aeme)
  
  # Check if elecations are within the range of inp$hypsograph$elevation
  for (name in names(elevation)) {
    if (elevation[[name]] == -1) next
    if (any(elevation[[name]] < min(inp$hypsograph$elev)) ||
            any(elevation[[name]] > max(inp$hypsograph$elev))) {
      cli::cli_abort("Elevation for {name} is out of range of hypsograph 
                     elevation")
    }
  }
  
  
  outf$data <- data
  outf$elevation <- elevation
  outf$factor <- factor
  outflows(aeme) <- outf
  return(aeme)
}
