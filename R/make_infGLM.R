#' write and configure inflows for a GLM simulation
#'
#' @param glm_nml
#' @param path_glm
#' @param list_inf
#' @param mass
#' @param key_naming
#' @param inf_factor
#'
#' @return
#' @noRd
#'
#' @importFrom glmtools set_nml
make_infGLM <- function(glm_nml, path_glm, list_inf, mass = TRUE,
                        inf_factor = 1) {

  # get the inflow attributes
  names_inf <- names(list_inf)
  n_inf <- length(names_inf)

  # check all the names are identical
  heads_inf <- lapply(list_inf, colnames)
  if (isFALSE(all(sapply(heads_inf, FUN = identical, heads_inf[[1]])))) {

    warning("headers for all inflows are not identical")
    print(heads_inf)
  }

  if (n_inf > 0) {
    # write GLM names to the inflow list,. and convert the units if needs be
    for (i in 1:length(list_inf)) {

      # format the tables
      df <- list_inf[[i]]
      colnames(df) <- rename_modelvars(colnames(df), type_output = "glm_aed")
      df <- df |>
        dplyr::mutate(flow = (flow * inf_factor) / 86400)


      # unit conversions
      if (isTRUE(mass)) {

        for (c in colnames(df)[-1]) {

          mult <- key_naming |>
            dplyr::filter(glm_aed == c) |>
            dplyr::pull(conversion_aed)

          df[, c] <- round(df[, c] / mult, 5)
        }

        list_inf[[i]] <- df
      }
    }
    # write the files
    mapply(list_inf, FUN = write.csv, file = file.path(path_glm,
                                                       paste0("bcs/inflow_",
                                                       names_inf, ".csv")),
           row.names = FALSE, quote = FALSE)

    arg_list <- list(num_inflows = n_inf, names_of_strms = names_inf,
                     strm_hf_angle = rep(80, n_inf),
                     strmbd_slope = rep(0.5, n_inf),
                     strmbd_drag = rep(0.016, n_inf),
                     inflow_factor = rep(1, n_inf),
                     inflow_fl = paste0("bcs/inflow_", names_inf,
                                        ".csv", collapse = ", "),
                     inflow_varnum = length(names(df)[2:ncol(df)]),
                     inflow_vars = names(df)[2:ncol(df)],
                     coef_inf_entrain = 0
                     )

    glm_nml <- glmtools::set_nml(glm_nml = glm_nml, arg_list = arg_list)
  } else {
    glm_nml[["inflow"]] <- NULL
  }

  return(glm_nml)
}
