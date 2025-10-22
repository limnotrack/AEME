#' Set inflow data from Aeme object
#' 
#' Replace existing inflow data in an Aeme object with new inflow data. The new
#' inflow data can be provided as either a named list of data frames or a single
#' data frame containing an "inflow_id" column to identify different inflows.
#'
#' @inheritParams build_aeme
#' @param inflows Either a named list of data frames for each inflow, or a 
#' single data frame containing an "inflow_id" column to identify different 
#' inflows. Each data frame must have columns "Date", "HYD_flow", "HYD_temp" and
#' "CHM_salt".
#'
#' @returns Aeme object with inflows replaced
#' 
#' @importFrom dplyr bind_rows
#' @export

set_inflows <- function(aeme, inflows) {
  inf <- aeme |> 
    check_aeme() |> 
    inflows() 
  inf_list <- inf[["data"]]
  curr_names <- names(inf_list)
  if (is.list(inflows)) {
    new_names <- names(inflows)
    if (is.null(new_names) || any(new_names == "")) {
      stop("If 'inflows' is a list, it must be a named list.")
    }
  } else if (is.data.frame(inflows)) {
    # Check if "inflow_id" column exists
    if (!"inflow_id" %in% colnames(inflows)) {
      stop("If 'inflows' is a data frame, it must contain an 'inflow_id' column.")
    }
    # Convert data frame to list and drop "inflow_id" column and use as list name
    inflows <- split(inflows, inflows[["inflow_id"]])
    inflows <- lapply(inflows, \(df) {
      df[ , colnames(df) != "inflow_id", drop = FALSE]
    })
    new_names <- names(inflows)
  } else {
    stop("'inflows' must be either a data frame or a named list of data frames.")
  }
  
  # Overwrite original inflows with new inflows
  inf[["data"]] <- inflows
  # If precipitation inflow exists in original inflows, keep it
  # if ("precip" %in% curr_names) {
  #   inf[["data"]][["precip"]] <- inf_list[["precip"]]
  # }
  
  inflows(aeme) <- inf
  return(aeme)
}
