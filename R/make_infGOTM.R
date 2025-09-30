#' Write GOTM inflow files and update GOTM yaml file
#'
#' @inheritParams make_yamlGOTM
#' @param inf_list list of dataframes with inflow data.
#' @param update_gotm logical, if TRUE update the GOTM yaml file.
#'
#' @importFrom dplyr select rename mutate
#' @importFrom utils write.table
#'
#' @return write updated GOTM yaml file in GOTM directory.
#' @noRd
make_infGOTM <- function(inf_list, inf_factor = 1, path_gotm, gotm = NULL,
                         update_gotm = TRUE, use_bgc) {

  names_inf <- names(inf_list)
  # Check if wbal is in the inflows
  if ("wbal" %in% names_inf) {
    inf_list[["inf_water_bal"]] <-  inf_list[["wbal"]] |>
      dplyr::select(-c(inflow_glm_aed, inflow_dy_cd)) |>
      dplyr::rename(HYD_flow = inflow_gotm_wet)
    inf_list[["wbal"]] <- NULL
    names_inf <- gsub("wbal", "inf_water_bal", names_inf)
  }

  if (length(names_inf) > 0) {
    for (f in 1:length(names_inf)) {

      df <- inf_list[[f]]

      if("NCS_ss2" %in% colnames(df)) {
        df$NCS_ss2 <- NULL
      }

      colnames(df) <- rename_modelvars(colnames(df), type_output = "gotm_wet")
      # Remove columns with no name - not necessary for GLM
      df <- df[, colnames(df) != ""]

      df <- df |>
        dplyr::mutate(time = "12:00:00",
                      flow = (flow * inf_factor) / 86400) |>
        dplyr::select(c("date", "time", everything()))

      utils::write.table(df[, c("date", "time", "flow")],
                         file.path(path_gotm, "inputs",
                                   paste0("inf_flow_", names_inf[f],".dat")),
                         row.names = FALSE, col.names = FALSE, quote = FALSE, na = "",
                         sep = "\t")

      ## write the temperature file
      utils::write.table(df[, c("date", "time", "temp")],
                         file.path(path_gotm, "inputs",
                                   paste0("inf_temp_", names_inf[f],".dat")),
                         row.names = FALSE, col.names = FALSE, quote = FALSE, na = "",
                         sep = "\t")
      ## write the salinity file
      utils::write.table(df[, c("date", "time", "salt")],
                         file.path(path_gotm, "inputs",
                                   paste0("inf_salt_", names_inf[f],".dat")),
                         row.names = FALSE, col.names = FALSE, quote = FALSE, na = "",
                         sep = "\t")

      inf_lst <- lapply(c("flow", "temp", "salt"), \(x) {
        list(method = 2, constant_value = 0,
             file = paste0("inputs/inf_", x, "_", names_inf[f],".dat"),
             column = 1, scale_factor = 1, offset = 0)
      })
      names(inf_lst) <- c("flow", "temp", "salt")

      bgc_vars <- c("abiotic_water_sO2W",
                    "abiotic_water_sPO4W", "abiotic_water_sPDOMW",
                    "abiotic_water_sPPOMW", "abiotic_water_sPAIMW",
                    "abiotic_water_sNH4W", "abiotic_water_sNO3W",
                    "abiotic_water_sNDOMW", "abiotic_water_sNPOMW",
                    "abiotic_water_sDIMW","abiotic_water_sDDOMW",
                    "abiotic_water_sDPOMW", "abiotic_water_sSiO2W")

      if (use_bgc & all(bgc_vars %in% colnames(df))) {
        # write the chemistry file (pclake can't have phytoplnkton in inflows?!?!)
        all.inf.vars = c("date","time", "abiotic_water_sO2W",
                         "abiotic_water_sPO4W", "abiotic_water_sPDOMW",
                         "abiotic_water_sPPOMW", "abiotic_water_sPAIMW",
                         "abiotic_water_sNH4W", "abiotic_water_sNO3W",
                         "abiotic_water_sNDOMW", "abiotic_water_sNPOMW",
                         "abiotic_water_sDIMW","abiotic_water_sDDOMW",
                         "abiotic_water_sDPOMW", "abiotic_water_sSiO2W")

        df.chem <- df |>
          dplyr::select(contains(all.inf.vars))

        df.chem <- df.chem |>
          dplyr::mutate(dplyr::across(3:ncol(df.chem), \(x) round(x, 4)))

        utils::write.table(df.chem,
                           file.path(path_gotm, "inputs",
                                     paste0("inf_chem_", names_inf[f],".dat")),
                           row.names = FALSE, col.names = FALSE, quote = FALSE, na = "",
                           sep = "\t")

        if (update_gotm) {
          chem_lst <- lapply(3:ncol(df.chem), \(c) {
            list(method = 2, constant_value = 0,
                 file = paste0("inputs/inf_chem_",names_inf[f],".dat"),
                 column = c - 2, scale_factor = 1, offset = 0)
          })
          names(chem_lst) <- names(df.chem)[-c(1, 2)]

          gotm[["streams"]][[names_inf[f]]] <- c(list(method = 1, zu = 0, zl = -1),
                                                 inf_lst, chem_lst)
        }
      } else {
        if (update_gotm) {
          gotm[["streams"]][[names_inf[f]]] <- c(list(method = 1, zu = 0, zl = -1),
                                                 inf_lst)
        }
      }
    }
  }
  if (update_gotm) {
    return(gotm)
  }
}
