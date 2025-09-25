#' Get variable summary
#'
#' @inheritParams get_var
#'
#' @return A data frame
#' @noRd
get_var_summary <- function(aeme, model, var_sim, ens_n = 1) {

  var_sim <- check_aeme_vars(var_sim)
  outp <- output(aeme)
  ens_lab <- paste0("ens_", sprintf("%03d", ens_n))
  names(model) <- model


  lst <- lapply(model, \(m) {
    variable <- outp[[ens_lab]][[m]][[var_sim]]
    # Empty dataframe to return if variable is not in output
    df <- data.frame(Date = as.Date(NA),
                     lyr_top = NA,
                     value = NA,
                     Model = m,
                     lyr_thk = NA)

    if (is.matrix(variable)) {
      df_top <- data.frame(Date = outp[[ens_lab]][[m]][["Date"]],
                           lyr_top = NA,
                           value = variable[, 1],
                           Model = m,
                           lyr_thk = NA,
                           var_sim = paste0(var_sim, "_top"))
      df_btm <- data.frame(Date = outp[[ens_lab]][[m]][["Date"]],
                           lyr_top = NA,
                           value = variable[, 2],
                           Model = m,
                           lyr_thk = NA,
                           var_sim = paste0(var_sim, "_btm"))
      df <- rbind(df_top, df_btm)
      return(df)
    }
  }) |>
    dplyr::bind_rows()

  return(lst)

}
