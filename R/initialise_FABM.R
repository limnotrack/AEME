#' Initialise FABM
#'
#' @param path_gotm filepath; to GOTM model setup
#' @inheritParams build_ensemble
#'
#' @importFrom dplyr filter
#' @importFrom yaml read_yaml
#'
#' @return NULL
#' @noRd
#'

initialise_FABM <- function(path_gotm, mod_ctrls) {

  fabm <- yaml::read_yaml(file.path(path_gotm, "fabm.yaml"))

  this_ctrls <- mod_ctrls |>
    dplyr::filter(simulate == 1,
                  !name %in% c("DateTime","HYD_flow","HYD_temp","HYD_dens",
                               "RAD_par","RAD_extc","RAD_secchi",
                               "CHM_salt",
                               "PHS_pip", "NIT_pin",
                               "PHS_tp","NIT_tn","PHY_tchla")
    )

  nme_chk <- rename_modelvars(input = this_ctrls$name,
                              type_output = "gotm_fabm")
  # Remove columns with no name - not necessary for GLM
  this_ctrls <- this_ctrls[nme_chk != "", ]
  params <- rename_modelvars(input = this_ctrls$name, type_output = "gotm_fabm")

  if (sum(is.na(this_ctrls$initial_wc)) > 0) {
    stop("incomplete initialisation, please check your key file")
  }

  # iterate through the state variables
  for (i in 1:length(params)) {
    key <- strsplit(params[i], "/")[[1]]
    if (length(key) == 4) {
      old_val <- fabm[[key[1]]][[key[2]]][[key[3]]][[key[4]]]
      new_val <- this_ctrls$initial_wc[i]
      message(paste0(params[i], " ", paste0(old_val,
                                            " replaced with ", new_val)))
      fabm[[key[1]]][[key[2]]][[key[3]]][[key[4]]] <- this_ctrls$initial_wc[i]
    }

    # Update N & P for phytos - Redfield ratio
    if (key[2] %in% c("cyanobacteria", "greens", "diatoms")) {
      fabm[[key[1]]][[key[2]]][[key[3]]][["sNW"]] <- this_ctrls$initial_wc[i] /
                                           16
      fabm[[key[1]]][[key[2]]][[key[3]]][["sPW"]] <- this_ctrls$initial_wc[i] /
        0.1
    }
  }

  # write the file
  write_yaml(fabm, file.path(path_gotm, "fabm.yaml"))
}

