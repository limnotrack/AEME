#' Find simulated variables missing an initial value
#'
#' @param model_controls data frame of model controls
#'
#' @returns character vector of var_aeme values set to simulate but missing
#'   an initial water column or sediment value
#' @noRd
check_model_controls_initial_values <- function(model_controls) {
  is_true_vec <- function(x) !is.na(x) & x
  sim <- model_controls[is_true_vec(model_controls$simulate), , drop = FALSE]
  sim$var_aeme[is.na(sim$initial_wc) | is.na(sim$initial_sed)]
}

#' Set model controls for AEME object
#'
#' @inheritParams build_aeme
#'
#' @returns Aeme object with model controls set
#' @export

set_model_controls <- function(aeme, model_controls) {
  aeme <- check_aeme(aeme)
  if (!is.data.frame(model_controls)) {
    stop("model_controls must be a data frame.")
  }
  mod_ctrls_names <- c("var_aeme", "simulate", "inf_default", "initial_wc",
                       "initial_sed", "conversion_aed")
  if (!all(mod_ctrls_names %in% names(model_controls))) {
    stop(paste("model_controls must contain the following columns:",
               paste(mod_ctrls_names, collapse = ", ")))
  }
  problems <- check_model_controls_initial_values(model_controls)
  if (length(problems) > 0) {
    cli::cli_warn(c(
      "!" = paste("The following variables are set to simulate but are",
                  "missing an initial water column or sediment value:"),
      "i" = paste(problems, collapse = ", ")
    ))
  }

  config <- configuration(aeme)
  config$model_controls <- new_model_controls(model_controls)
  configuration(aeme) <- config
  return(aeme)
}
