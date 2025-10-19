#' Get modeled observation variables
#'
#' @param aeme Aeme object
#'
#' @returns Data frame with modeled observation variables and summary 
#' statistics.
#' 
#' @importFrom dplyr filter mutate group_by summarise pull
#' 
#' @export
#'

get_mod_obs_vars <- function(aeme) {
  tme <- time(aeme)
  out_vars <- get_output_vars(aeme)
  if (is.null(out_vars)) {
    model_controls <- get_model_controls(aeme)
    if (!is.null(model_controls)) {
      out_vars <- model_controls |> 
        dplyr::filter(simulate) |> 
        dplyr::pull(var_aeme)
    }
  }
  obs <- get_obs(aeme) |> 
    dplyr::filter(Date >= as.Date(tme$start) & Date <= as.Date(tme$stop)) |> 
    dplyr::mutate(depth_mid = (depth_from + depth_to) / 2) |> 
    dplyr::group_by(var_aeme) |>
    dplyr::summarise(
      n = dplyr::n(),
      n_depth = length(unique(depth_mid)),
      n_dates = length(unique(Date)),
      .groups = "drop"
    )
  
  if (!is.null(out_vars)) {
    obs <- obs |>
      dplyr::filter(var_aeme %in% out_vars)
  }
  return(obs)
}
