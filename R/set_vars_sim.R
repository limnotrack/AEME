#' Set simulation variables in model controls
#'
#' @inheritParams build_aeme
#' @param vars_sim character vector of variable names to set for simulation
#' @param simulate logical, whether to simulate the variables in vars_sim
#' @param exclusive logical, if TRUE, set all other variables to not simulate
#'
#' @importFrom dplyr bind_rows arrange desc
#' @importFrom cli cli_alert_info
#' @returns Updated model_controls data frame
#' @export
#'
#' @examples
#' model_controls <- data.frame(
#'  var_aeme = c("HYD_temp", "CHM_oxy", "PHS_tp"),
#'  simulate = c(TRUE, FALSE, TRUE),
#'  inf_default = c(NA, NA, NA),
#'  initial_wc = c(NA, NA, NA),
#'  initial_sed = c(NA, NA, NA),
#'  conversion_aed = c(1, 1, 1),
#'  stringsAsFactors = FALSE
#' )
#' vars_sim <- c("CHM_oxy", "NIT_tn", "PHS_tp")
#' updated_controls <- set_vars_sim(model_controls, vars_sim, simulate = TRUE)
#' print(updated_controls)
#' 
set_vars_sim <- function(model_controls,
                         vars_sim,
                         simulate = TRUE,
                         exclusive = FALSE) {
  
  stopifnot("var_aeme" %in% names(model_controls),
            "simulate" %in% names(model_controls))
  
  # Handle exclusive mode
  if (exclusive) {
    model_controls$simulate <- FALSE
  }
  
  # Find matches
  idx <- match(vars_sim, model_controls$var_aeme)
  
  # Warn about missing variables & add them to model_controls if needed
  missing <- is.na(idx)
  # Apply simulate flag only to those found
  model_controls$simulate[idx[!missing]] <- simulate
  if (any(missing)) {
    missing_vars <- vars_sim[missing]
    msg <- paste("Variables not found:", paste(missing_vars, collapse = ", "), 
                 ". Adding them to model_controls.")
    cli_inform_safe(c("i" = msg))
    new_rows <- data.frame(
      var_aeme = missing_vars,
      simulate = simulate,
      inf_default = NA_real_,
      initial_wc = NA_real_,
      initial_sed = NA_real_,
      conversion_aed = 1,
      stringsAsFactors = FALSE
    )
    model_controls <- dplyr::bind_rows(model_controls, new_rows)
  }
  model_controls <- model_controls |>
    dplyr::arrange(dplyr::desc(simulate), var_aeme)

  return(new_model_controls(model_controls))
}
