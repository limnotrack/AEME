#' Get inflow data from Aeme object
#'
#' @inheritParams build_aeme
#' @param return_df Logical. If TRUE, returns a single data frame with all 
#' inflows. If FALSE, returns a list of data frames for each inflow. Default is 
#' FALSE.
#'
#' @returns A list of data frames or a single data frame with all inflows
#' combined.
#' 
#' @importFrom dplyr bind_rows
#' @export

get_inflows <- function(aeme, return_df = FALSE) {
  inf <- aeme |> 
    check_aeme() |> 
    inflows() 
  inf_list <- inf[["data"]]
  if (!return_df) {
    return(inf_list)
  } else {
    inf_names <- names(inf_list)
    inf_df <- lapply(inf_names, function(x) {
      df <- inf_list[[x]]
      df[["inflow_id"]] <- x
      return(df)
    }) |> 
      dplyr::bind_rows()
    return(inf_df)
  }
}
