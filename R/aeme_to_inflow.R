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
  tme <- time(aeme)
  spin_up <- lapply(model, \(m) {
    list(start = lubridate::as_date(tme$start - 
                                      lubridate::ddays(tme$spin_up[[m]])),
         end = lubridate::as_date(tme$stop)
         )
  })
  names(spin_up) <- model
  outflow_factor <- rep(1, length(model))
  names(outflow_factor) <- model
  
  model_parameters <- parameters(aeme)
  if (!is.null(model_parameters)) {
    # Get outflow factor if present
    outflow_fct <- model_parameters |> 
      dplyr::filter(name == "outflow", file == "wdr") |> 
      dplyr::select(model, value) 
    for (m in model) {
      if (m %in% outflow_fct$model) {
        outflow_factor[m] <- outflow_fct |> 
          dplyr::filter(model == m) |> 
          dplyr::pull(value)
      }
    }
  }
  
  inflow_vars <- c(
    "LKE_outflow",
    "HYD_temp", "CHM_salt", "CHM_oxy",
    "NIT_tn", "NIT_nit", "NIT_amm", #"NIT_din",
    "PHS_tp", "PHS_frp",# "NCS_tss", "NCS_ss1",
    "NIT_don", "NIT_pon", "PHS_dop", "PHS_pop", "PHS_pip",
    "CAR_doc", "CAR_poc"
  )
  var_check <- check_var_in_output(aeme = aeme, model = model, 
                                   var_sim = inflow_vars)
  
  out <- lapply(model, \(m) {
    df <- lapply(inflow_vars, \(var) {
      
      if (var_check[[m]][var] == FALSE) {
        return(NULL)
      }
      dat <- get_var(aeme = aeme, model = m, var_sim = var, depth = 0.25,
                     remove_spin_up = FALSE)
      mean_value <- dat |>
        dplyr::filter(Date >= lubridate::as_date(tme$start) &
                        Date < lubridate::as_date(tme$start) + 3) |>
        dplyr::pull(value) |> 
        mean(na.rm = TRUE)
      dat_missing <- data.frame(Date = (dat$Date[1] - lubridate::ddays(1)),
                                var_sim = var,
                                value = mean_value)
      dat <- dplyr::bind_rows(dat_missing, dat)
      if (var == "LKE_outflow") {
        dat <- dat |> 
          dplyr::mutate(value = value * outflow_factor[m])
      }
      if (all(is.na(dat$value)) | all(dat$value < 0)) {
        return(NULL)
      } else {
        return(dat)
      }
    }) |> 
      dplyr::bind_rows()
    if (nrow(df) == 0) {
      return(NULL)
    }
    df <- df |>
      dplyr::select(Date, var_sim, value)
    df_wid <- df |> 
      tidyr::pivot_wider(names_from = var_sim, values_from = value) |> 
      dplyr::rename(HYD_flow = LKE_outflow) |>
      dplyr::mutate(model = m)
    return(df_wid)
  }) |> 
    dplyr::bind_rows()
  return(out)
}
