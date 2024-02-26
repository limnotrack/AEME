#' Get model controls
#'
#' @inheritParams build_ensemble
#'
#' @return A data frame of model controls
#' @export
#'
#' @examples
#' \dontrun{
#' model_controls <- get_model_controls()
#' }
#'

get_model_controls <- function(use_bgc = FALSE) {
  utils::data("model_controls", package = "AEME")
  hyd_vars <- c("HYD_temp", "CHM_salt", "CHM_oxy")
  bgc_vars <- c("PHS_frp", "PHS_dop", "PHS_pop", "PHS_pip", "PHS_tp",
                "NIT_amm", "NIT_nit", "NIT_don", "NIT_pon", "NIT_tn",
                "CAR_doc", "CAR_poc",
                "SIL_rsi",
                "PHY_cyano", "PHY_green", "PHY_crypt", "PHY_diatom",
                "PHY_tchla",
                "NCS_ss1"
                )
  if (use_bgc) {
    model_controls <- model_controls |>
      dplyr::mutate(simulate = ifelse(var_aeme %in% c(hyd_vars, bgc_vars),
                                      TRUE, FALSE))

  } else {
    model_controls <- model_controls |>
      dplyr::mutate(simulate = ifelse(var_aeme %in% c(hyd_vars), TRUE, FALSE))
  }
  return(model_controls)
}
