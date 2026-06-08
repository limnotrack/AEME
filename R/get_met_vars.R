#' Get the names and units for the meteorological variables
#' 
#' @param as_vector Logical. If TRUE, returns a named vector with column names 
#' for the meteorological variables. If FALSE, returns a data frame with v
#' ariable names, column names and units. Default is FALSE.
#'
#' @returns Named vector with column names for the meteorological variables. 
#' The names of the vector are the variable names in the Aeme object and the 
#' values are the corresponding column names in the input data frame.
#' @export
#' 
#' @importFrom dplyr filter
#'
#' @examples
#' get_met_vars()
#' 
#' # Get the column names for the meteorological variables as a named vector
#' met_cols <- get_met_vars(as_vector = TRUE)
#' 
get_met_vars <- function(as_vector = FALSE) {
  met_vars <- key_naming |> 
    dplyr::filter(grepl("^(Date|MET_)", .data$var_aeme)) |> 
    dplyr::select(var_aeme, name_text, units)
  
  if (as_vector) {
    met_cols <- met_vars[["var_aeme"]]
    names(met_cols) <- met_vars[["name_text"]]
    return(met_cols)
  } else {
    return(met_vars)
  }
}
