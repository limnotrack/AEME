#' Get AEME parameters data frame column names
#' @param incl_opt Logical; if TRUE, include optional parameter names. Default 
#' is TRUE.
#' @return A character vector of AEME parameter data frame column names.
#' @export
param_colnames <- function(incl_opt = TRUE) {
  key_names <- c("model", "file", "name", "value", "min", "max", "group",
                 "index")
  opt_names <- c("module", "var_sim", "par", "logical", "logical_val",
                 "char", "char_val")
  if (incl_opt) {
    param_names <- c(key_names, opt_names)
  } else {
    param_names <- key_names
  }
  return(param_names)
}
