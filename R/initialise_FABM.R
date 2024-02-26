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

initialise_FABM <- function(path_gotm, model_controls) {

  fabm <- yaml::read_yaml(file.path(path_gotm, "fabm.yaml"))

  this_ctrls <- model_controls |>
    dplyr::filter(simulate,
                  !var_aeme %in% c("DateTime","HYD_flow","HYD_temp","HYD_dens",
                               "RAD_par","RAD_extc","RAD_secchi",
                               "CHM_salt",
                               "PHS_pip", "NIT_pin",
                               "PHS_tp","NIT_tn","PHY_tchla")
    )

  nme_chk <- rename_modelvars(input = this_ctrls$var_aeme,
                              type_output = "gotm_fabm")
  # Remove columns with no name - not necessary for GLM
  this_ctrls <- this_ctrls[nme_chk != "", ]
  params <- rename_modelvars(input = this_ctrls$var_aeme,
                             type_output = "gotm_fabm")

  if (sum(is.na(this_ctrls$initial_wc)) > 0) {
    stop("incomplete initialisation, please check your key file")
  }

  # iterate through the state variables
  for (i in 1:length(params)) {
    key <- strsplit(params[i], "/")[[1]]

    # Update N & P for phytos - Redfield ratio
    if (key[2] %in% c("cyanobacteria", "greens", "diatoms")) {
      cChDMin <- fabm[[key[1]]][[key[2]]][["parameters"]][["cChDMin"]]
      cChDMax <- fabm[[key[1]]][[key[2]]][["parameters"]][["cChDMax"]]
      rChD <- cChDMax - (cChDMax - cChDMin) # * aLLim
      mgPerg <- 1000
      # oChla = mgPerg * rChD * sDW
      # C:N:P ~ 106:16:1
      sDW <- signif(this_ctrls$initial_wc[i] / (mgPerg * rChD), 2)
      sNW <- signif(sDW * (16 / 106), 2)
      sPW <- signif(sDW * (1 / 106), 2)

      old_val <- fabm[[key[1]]][[key[2]]][[key[3]]][["sDW"]]
      message(paste0(paste0(key[1:3], collapse = "/"), "/sDW ",
                     paste0(old_val, " replaced with ", sDW)))
      fabm[[key[1]]][[key[2]]][[key[3]]][["sDW"]] <- sDW

      old_val <- fabm[[key[1]]][[key[2]]][[key[3]]][["sNW"]]
      message(paste0(paste0(key[1:3], collapse = "/"), "/sNW ",
                     paste0(old_val, " replaced with ", sNW)))
      fabm[[key[1]]][[key[2]]][[key[3]]][["sNW"]] <- sNW

      old_val <- fabm[[key[1]]][[key[2]]][[key[3]]][["sPW"]]
      message(paste0(paste0(key[1:3], collapse = "/"), "/sPW ",
                     paste0(old_val, " replaced with ", sPW)))
      fabm[[key[1]]][[key[2]]][[key[3]]][["sPW"]] <- sPW

    } else if (key[2] %in% c("cladocerans")) {
      cNDZooRef <- fabm[[key[1]]][[key[2]]][["parameters"]][["cNDZooRef"]]
      cPDZooRef <- fabm[[key[1]]][[key[2]]][["parameters"]][["cPDZooRef"]]
      sD <- this_ctrls$initial_wc[i]
      sN <- cNDZooRef * sD
      sP <- cPDZooRef * sD

      old_val <- fabm[[key[1]]][[key[2]]][[key[3]]][["sD"]]
      message(paste0(paste0(key[1:3], collapse = "/"), "/sD ",
                     paste0(old_val, " replaced with ", sD)))
      fabm[[key[1]]][[key[2]]][[key[3]]][["sD"]] <- sD

      old_val <- fabm[[key[1]]][[key[2]]][[key[3]]][["sN"]]
      message(paste0(paste0(key[1:3], collapse = "/"), "/sN ",
                     paste0(old_val, " replaced with ", sN)))
      fabm[[key[1]]][[key[2]]][[key[3]]][["sN"]] <- sN

      old_val <- fabm[[key[1]]][[key[2]]][[key[3]]][["sP"]]
      message(paste0(paste0(key[1:3], collapse = "/"), "/sP ",
                     paste0(old_val, " replaced with ", sP)))
      fabm[[key[1]]][[key[2]]][[key[3]]][["sP"]] <- sP
    } else {
      if (length(key) == 4) {
        old_val <- fabm[[key[1]]][[key[2]]][[key[3]]][[key[4]]]
        new_val <- this_ctrls$initial_wc[i]
        fabm[[key[1]]][[key[2]]][[key[3]]][[key[4]]] <- this_ctrls$initial_wc[i]
      }
      message(paste0(params[i], " ", paste0(old_val,
                                            " replaced with ", new_val)))
    }
  }

  # write the file
  write_yaml(fabm, file.path(path_gotm, "fabm.yaml"))
}
