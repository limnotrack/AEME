#' Get derived variables needed for simulation
#'
#' @param vars_sim Character vector of variable names being simulated
#'
#' @returns Character vector of variable names that are needed as inputs
#' @export
#'
#' @examples
#' get_deriv_inputs(vars_sim  = c("HYD_thmcln", "HYD_epidep", "CHM_oxyepi", "LKE_tli4"))
get_deriv_inputs <- function(vars_sim) {
  data("key_naming", package = "AEME", envir = environment())
  vars_sim <- check_aeme_vars(vars_sim)
  
  deriv_vars <- key_naming |> 
    dplyr::filter(var_aeme %in% vars_sim, derived)
  
  if (nrow(deriv_vars) == 0) {
    return(NULL)
  } else {
    deriv <- deriv_vars |> 
      dplyr::pull(derived_from) |> 
      strsplit(";\\s*") |>
      unlist() |> 
      unique()
    
    # Check for double deriatives
    more_deriv <- key_naming |> 
      dplyr::filter(var_aeme %in% deriv, derived) |> 
      dplyr::pull(derived_from)
    if (length(more_deriv) > 0) {
      more <- more_deriv |> 
        strsplit(";\\s*") |>
        unlist() |> 
        unique()
      deriv <- unique(c(deriv, more))
    }
    priority <- c("HYD", "CHM", "LKE")
    
    # Order variables
    ret_vars <- key_naming |>
      dplyr::filter(var_aeme %in% deriv) |>
      dplyr::mutate(group = sub("_.*$", "", var_aeme),
                    order  = match(group, priority)) |>
      dplyr::arrange(order, derived) |> 
      dplyr::pull(var_aeme)
    return(ret_vars)
  }
}
