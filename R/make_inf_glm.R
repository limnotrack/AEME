#' write and configure inflows for a GLM simulation
#'
#' @inheritParams set_nml
#' @inheritParams make_wdr_glm
#' @param path_glm filepath; to GLM directory
#' @param list_inf list of inflows
#' @param mass logical; do mass conversion for GLM-AED units.
#' @param inf_factor numeric; scaling factor to apply to inflows
#' @param aed_models character vector of active AED modules (see
#'   `glm_active_aed_models()`); inflow variables belonging to inactive modules
#'   are left out of `inflow_vars`. `NULL` applies no filter.
#'
#' @return nml object with updated inflow section
#' @noRd
#'
#' @importFrom utils write.csv

make_inf_glm <- function(glm_nml, path_glm, list_inf, mass = TRUE,
                        inf_factor = 1, update_nml = TRUE, aed_models = NULL) {

  # Load Rdata
  data("key_naming", package = "AEME", envir = environment())

  # get the inflow attributes
  names_inf <- names(list_inf)
  n_inf <- length(names_inf)

  # Check if wbal is in the inflows
  # if ("wbal" %in% names_inf) {
  #   list_inf[["wbal"]] <-  list_inf[["wbal"]] |>
  #     dplyr::select(-c(inflow_dy_cd, inflow_gotm_wet)) |>
  #     dplyr::rename(HYD_flow = inflow_glm_aed)
  # }
  for (i in 1:length(list_inf)) {
    if ("model" %in% colnames(list_inf[[i]])) {
      list_inf[[i]] <- list_inf[[i]] |>
        dplyr::filter(model == "glm_aed") |> 
        dplyr::select(-model)
    }
  }

  # check all the names are identical
  heads_inf <- lapply(list_inf, colnames)
  if (isFALSE(all(sapply(heads_inf, FUN = identical, heads_inf[[1]])))) {

    warning("Headers for all inflows are not identical")
    print(heads_inf)
  }

  if (n_inf > 0) {
    # write GLM names to the inflow list,. and convert the units if needs be
    for (i in 1:length(list_inf)) {

      # format the tables
      df <- list_inf[[i]]
      colnames(df) <- rename_modelvars(input = colnames(df),
                                       type_output = "glm_aed",
                                       passthrough = TRUE)
      # Remove columns with no name - not necessary for GLM
      df <- df[, colnames(df) != ""]

      df <- df |>
        dplyr::mutate(flow = (flow * inf_factor) / 86400)


      # unit conversions
      if (isTRUE(mass)) {

        for (c in colnames(df)[-1]) {

          mult <- key_naming |>
            dplyr::filter(glm_aed == c) |>
            dplyr::pull(conversion_aed)
          # variables passed through from outside key_naming: no conversion
          if (length(mult) == 0L) mult <- 1

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

    # Only list variables whose AED module is active; the CSVs keep every
    # column (GLM reads by header name) so modules can be re-enabled later.
    inf_vars <- names(df)[2:ncol(df)]
    dropped <- inf_vars[!glm_aed_var_active(inf_vars, aed_models)]
    if (length(dropped) > 0) {
      cli_inform_safe(c("i" = "Skipping inflow variables whose AED module is \\
                              not active: {paste(dropped, collapse = ', ')}."))
    }
    inf_vars <- setdiff(inf_vars, dropped)

    arg_list <- list(num_inflows = n_inf, names_of_strms = names_inf,
                     strm_hf_angle = rep(80, n_inf),
                     strmbd_slope = rep(0.5, n_inf),
                     strmbd_drag = rep(0.016, n_inf),
                     inflow_factor = rep(1, n_inf),
                     inflow_fl = paste0("bcs/inflow_", names_inf,
                                        ".csv"),
                     inflow_varnum = length(inf_vars),
                     inflow_vars = inf_vars,
                     coef_inf_entrain = 0
                     )
  } else {
    arg_list <- list(num_inflows = 0, names_of_strms = "none",
                     strm_hf_angle = rep(80, 1),
                     strmbd_slope = rep(0.5, 1),
                     strmbd_drag = rep(0.016, 1),
                     inflow_factor = rep(1, 1),
                     inflow_fl = "",
                     inflow_varnum = 3,
                     inflow_vars = c("flow", "temp", "salt"),
                     coef_inf_entrain = 0
    )
  }

  if (update_nml) {
    glm_nml <- set_nml(glm_nml = glm_nml, arg_list = arg_list)
    return(glm_nml)
  }
}
