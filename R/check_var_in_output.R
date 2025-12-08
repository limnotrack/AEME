#' Check if a variable is present in the model output
#'
#' @inheritParams build_aeme
#' @inheritParams plot_output
#' @param ens_n numeric; ensemble number
#' @return A list with logical matrices indicating if each variable is present per model,
#'   plus an overall `all_present` flag.
#' @export
#' @importFrom cli cli_warn
#' 
check_var_in_output <- function(aeme, model, var_sim, ens_n = 1) {
  # --- Input validation ---
  aeme    <- check_aeme(aeme)
  model   <- check_model(model)
  var_sim <- check_aeme_vars(var_sim)
  outp    <- output(aeme)
  ens_lab <- format_ens_label(ens_n = ens_n)
  
  # --- Per-model variable check ---
  out <- lapply(model, function(m) {
    present <- sapply(var_sim, function(v) is_var_present(outp[[ens_lab]][[m]], v))
    df <- as.data.frame(t(as.matrix(present)))
    row.names(df) <- m
    df
  })
  
  names(out) <- model
  out$all_present <- all(unlist(out))
  
  # --- Inform about missing variables ---
  if (!out$all_present) {
    no_vars <- sapply(model, function(m) {
      v <- out[[m]]
      missing_vars <- colnames(v)[!v]
      if (length(missing_vars)) {
        paste0(m, ": ", paste(missing_vars, collapse = ", "))
      } else {
        NULL
      }
    })
    no_vars <- no_vars[!vapply(no_vars, is.null, logical(1))]
    
    if (length(no_vars) > 0) {
      cli::cli_warn(c(
        "!" = "Some variables are missing or invalid in model output:",
        "i" = no_vars
      ))
    }
  } else {
    cli_inform_safe("All requested variables are present in the model output.")
  }
  
  return(out)
}


#' Check if a variable is present in a single model output
#'
#' @param out_model list; output for a single model (from output(aeme)[[ens_lab]][[model]])
#' @param var character; variable name to check
#' @return logical; TRUE if variable exists and is not all -99
#' @noRd
is_var_present <- function(out_model, var) {
  chk <- !is.null(out_model[[var]])
  if (chk) {
    chk <- !all(out_model[[var]] == -99)
  }
  if (is.na(chk)) chk <- FALSE
  return(chk)
}

