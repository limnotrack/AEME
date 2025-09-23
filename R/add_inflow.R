#' Set inflow data from Aeme object
#' 
#' Replace existing inflow data in an Aeme object with new inflow data. The new
#' inflow data can be provided as either a named list of data frames or a single
#' data frame containing an "inflow_id" column to identify different inflows.
#' If the original inflows include a "precip" inflow, it will be
#' retained in the updated inflows.
#'
#' @inheritParams build_aeme
#' @param inflow A data frame containing inflow data. Must include columns "Date",
#' "HYD_flow", "HYD_temp", and "CHM_salt". 
#' An "inflow_id" column can be included to identify the inflow or it can be
#' provided separately. If multiple unique inflow_ids are present, an error
#' will be raised.
#' @param inflow_id A string specifying the identifier for the inflow. If not
#' provided, the function will look for an "inflow_id" column in the inflow
#' data frame. If the column is present but contains multiple unique values, an
#' error will be raised.
#'
#' @returns Aeme object with inflow added
#' 
#' @export

add_inflow <- function(aeme, inflow, inflow_id) {
  if (missing(inflow_id)) {
    if ("inflow_id" %in% colnames(inflow)) {
      inflow_id <- unique(inflow$inflow_id)
      if (length(inflow_id) > 1) {
        stop("inflow_id column contains multiple unique values. Please provide a single inflow_id.")
      }
      inflow <- inflow |>
        subset(select = -c(inflow_id))
    } else {
      stop("inflow_id is required")
    }
  }
  inf <- aeme |> 
    check_aeme() |> 
    inflows() 
  inf_list <- inf[["data"]]
  curr_names <- names(inf_list)
  if (inflow_id %in% curr_names) {
    stop(paste0("inflow_id '", inflow_id, 
                "' already exists. Please choose a different inflow_id."))
  }
  inf_list[[inflow_id]] <- inflow
  inf[["data"]] <- inf_list
  inflows(aeme) <- inf
  return(aeme)
}
