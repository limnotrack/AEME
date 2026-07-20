#' Get all variables to be simulated, including those that are
#' derived from others.
#'
#' @param vars_sim vector of variable names to be simulated. If NULL, the
#'   variables are taken from model_controls where simulate == TRUE.
#' @inheritParams build_aeme
#'
#' @returns vector of variable names
#' @export
#'
#' @examples
#' data("model_controls", package = "AEME")
#' get_vars_sim(model_controls)
#' get_vars_sim("HYD_thmcln")
get_vars_sim <- function(vars_sim, aeme, model_controls) {
  data("key_naming", package = "AEME", envir = environment())
  
  if (missing(vars_sim)) {
    if (!missing(aeme)) {
      model_controls <- get_model_controls(aeme)
    } else {
      if (missing(model_controls)) {
        stop("Either aeme or model_controls must be provided")
      }
    }
    vars_sim <- model_controls |> 
      dplyr::filter(simulate) |> 
      dplyr::arrange(var_aeme) |> 
      dplyr::pull(var_aeme)
  }
  
  deriv_vars <- key_naming |> 
    dplyr::filter(var_aeme %in% vars_sim, derived)
  
  if (nrow(deriv_vars) == 0) {
    return(vars_sim)
  } else {
    deriv <- deriv_vars |> 
      dplyr::pull(derived_from) |> 
      strsplit(";\\s*") |>
      unlist() |> 
      unique()
    
    # Check for double deriatives
    more_deriv <- key_naming |> 
      dplyr::filter(var_aeme %in% deriv, derived)
    if (nrow(more_deriv) > 0) {
      more <- more_deriv |> 
        dplyr::pull(derived_from) |> 
        strsplit(";\\s*") |>
        unlist() |> 
        unique()
      deriv <- unique(c(deriv, more))
    }
    all_vars <- unique(c(vars_sim, deriv)) 
    priority <- c("HYD", "CHM", "LKE")
    
    # Order variables
    ret_vars <- key_naming |>
      dplyr::filter(var_aeme %in% all_vars) |>
      dplyr::mutate(group = sub("_.*$", "", var_aeme),
                    order  = match(group, priority)) |>
      dplyr::arrange(order, derived) |> 
      dplyr::pull(var_aeme)
    
    return(ret_vars)
  }
}
