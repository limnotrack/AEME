#' Convert AEME model outputs to inflow format
#'
#' @inheritParams build_aeme
#'
#' @returns List of data frames with inflow variables for each model
#' @export
#'
#' @importFrom dplyr select mutate rename bind_rows
#' @importFrom tidyr pivot_wider
#'

aeme_to_inflow <- function(aeme) {
  aeme <- check_aeme(aeme)
  model <- list_models(aeme = aeme)
  
  inflow_vars <- c(
    # "HYD_flow", 
    "LKE_outflow",
    "HYD_temp", "CHM_salt", "CHM_oxy",
    "NIT_tn", "NIT_nit", "NIT_amm", #"NIT_din",
    "PHS_tp", "PHS_frp",# "NCS_tss", "NCS_ss1",
    "NIT_don", "NIT_pon", "PHS_dop", "PHS_pop", "PHS_pip",
    "CAR_doc", "CAR_poc"
  )
  
  out <- lapply(model, \(m) {
    df <- lapply(inflow_vars, \(var) {
      dat <- get_var(aeme = aeme, model = m, var_sim = var, depth = 0.25,
                     remove_spin_up = FALSE)
      if (all(is.na(dat$value)) | all(dat$value < 0)) {
        return(NULL)
      } else {
        return(dat)
      }
    }) |> 
      dplyr::bind_rows() |>
      dplyr::select(Date, var_sim, value)
    df_wid <- df |> 
      tidyr::pivot_wider(names_from = var_sim, values_from = value) |> 
      dplyr::rename(HYD_flow = LKE_outflow) |>
      dplyr::mutate(model = m)
    return(df_wid)
  }) 
  names(out) <- model
  return(out)
}
