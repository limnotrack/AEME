#' Check if a variable is present in the model output
#'
#' @inheritParams build_aeme
#'
#' @export
#'
#' @return A logical matrix indicating if the variable is present in the model

check_var_in_output <- function(aeme, model, var_sim, ens_n = 1) {
  outp <- output(aeme)
  ens_lab <- paste0("ens_", sprintf("%03d", ens_n))
  out <- lapply(model, \(m) {
    d <- sapply(var_sim, \(v) {
      chk <- !is.null(outp[[ens_lab]][[m]][[v]])
      if (chk) {
        chk <- !all(outp[[ens_lab]][[m]][[v]] == -99)
      }
      chk
    })
    d <- t(as.matrix(d))
    row.names(d) <- m
    d
    # t(data.frame(d)) |>
    #   as.data.frame() |>
    #   dplyr::mutate(model = m)
  })
  names(out) <- model
  out$all_present <- all(unlist(out))

  if (!out$all_present) {
    no_vars <- sapply(model, \(m) {
      v <- out[[m]]
      paste0(m, ": ", paste0(colnames(v)[!v], collapse = ", "))
    })
    message("No variables in model output for:\n",
            paste0(no_vars, collapse = "\n"))

  }


  return(out)
}
