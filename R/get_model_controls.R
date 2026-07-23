#' Get model controls
#'
#' @inheritParams build_aeme
#'
#' @return A data frame of model controls
#' @export
#'
#' @examples
#' \dontrun{
#' model_controls <- get_model_controls()
#' }
#'

get_model_controls <- function(aeme = NULL, use_bgc = FALSE) {
  
  if (!is.null(aeme)) {
    config <- configuration(aeme)
    model_controls <- config$model_controls
  } else {
    hyd_vars <- c("HYD_temp", "HYD_dens", "HYD_thmcln", "HYD_strat", "CHM_salt")
    light_vars <- c("RAD_par", "RAD_extc")
    bgc_vars <- c(
      "CHM_oxy",# "CHM_ph", 
      "PHS_frp", "PHS_dop", "PHS_pop", "PHS_pip", "PHS_tp",
      "NIT_amm", "NIT_nit", "NIT_don", "NIT_pon", "NIT_tn",
      "CAR_doc", "CAR_poc",
      "SIL_rsi",
      "PHY_cyano", "PHY_green", "PHY_diatom",
      "PHY_tchla",
      "NCS_ss1"
    )
    if (use_bgc) {
      sel_vars <- c(hyd_vars, light_vars, bgc_vars)
    } else {
      sel_vars <- c(hyd_vars, light_vars)
    }
    model_controls <- set_vars_sim(model_controls = model_controls,
                                   vars_sim = sel_vars, simulate = TRUE, 
                                   exclusive = TRUE)
    
  }
  return(model_controls)
}
