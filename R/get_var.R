#' Get variable from aeme_data
#'
#' @inheritParams plot_output
#' @param return_df logical; if TRUE, return a dataframe; if FALSE, return a
#' list. Default is TRUE.
#' @param cumulative logical; if TRUE, return cumulative sum of variable
#'
#' @return dataframe or list
#' @export

get_var <- function(aeme_data, model, var_sim, return_df = TRUE,
                    cumulative = FALSE) {

  # Extract output from aeme_data ----
  outp <- output(aeme_data)
  names(model) <- model

  # Loop through the models and extract the variable of interest ----
  lst <- lapply(model, \(m) {
    variable <- outp[[m]][[var_sim]]

    if (is.null(variable)) {
      message(strwrap(paste0(var_sim, " is not in output for model ", m,
                             ". Returning a dataframe with NA's.")))
      df <- data.frame(Date = NA, value = NA, Model = NA, lyr_thk = NA)
    } else if (length(dim(variable)) == 1 | is.numeric(variable)) {
      df <- data.frame(Date = outp[[m]][["Date"]],
                       lyr_top = NA,
                       value = variable,
                       Model = m,
                       lyr_thk = NA)
      if (cumulative) {
        df <- df |>
          dplyr::mutate(value = cumsum(value))
      }
    } else {
      depth <- data.frame(Date = outp[[m]][["Date"]],
                          depth = outp[[m]][["LKE_lvlwtr"]])
      lyr <- outp[[m]][["LAYERS"]]
      df <- data.frame(Date = rep(outp[[m]][["Date"]], each = nrow(variable)),
                       lyr_top = unlist(lyr),
                       value = unlist(variable),
                       Model = m) |>
        dplyr::mutate(lyr_thk = ifelse(c(-999, diff(lyr_top)) < 0, # | is.na(-c(NA,diff(lyr_top))),
                                       c(diff(lyr_top),NA),
                                       c(NA,diff(lyr_top))))
      if (cumulative) {
        warning("Applying cumulative to a value with a depth component.")
        df <- df |>
          dplyr::mutate(value = cumsum(value))
      }
    }
    df
  })

  if (return_df) {
    do.call(rbind, lst) |>
      dplyr::mutate(Model = dplyr::case_when(
        Model == "dy_cd" ~ "DYRESM-CAEDYM",
        Model == "glm_aed" ~ "GLM-AED",
        Model == "gotm_wet" ~ "GOTM-WET"
        ),
        var_sim = var_sim
      )
  } else {
    lst
  }
}
