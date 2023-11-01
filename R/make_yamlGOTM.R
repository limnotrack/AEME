#' Modify the GOTM yaml file for ensemble simulation
#'
#' @inheritParams delagrangify
#' @inheritParams initialiseGLM
#' @inheritParams initialiseGOTM
#' @inheritParams build_dycd
#' @inheritParams build_ensemble
#'
#' @return write updated GOTM yaml file in GOTM directory.
#' @noRd

make_yamlGOTM <- function(lakename, date_range, hyps, gps, nlev, met, inf,
                          outf, init_depth, path_gotm, ext_elev,
                          outf_factor, inf_factor, Kw, use_bgc, hum_type = 1) {

  met_ref <- data.frame(gotm = c("u10", "v10", "airp", "airt", "hum", "hum",
                                 "cloud", "swr", "precip"),
                        std = c("MET_wnduvu", "MET_wnduvv", "MET_prsttn",
                                "MET_tmpair", "MET_tmpdew", "MET_humrel",
                                "MET_cldcvr", "MET_radswd", "precip"))

  gotm <- yaml::read_yaml(file.path(path_gotm, "gotm.yaml"))

  gotm$location$name <- lakename
  gotm$location$latitude <- round(gps[2], 5)
  gotm$location$longitude <- round(gps[1], 5)
  gotm$location$depth <- max(hyps$elev) - min(hyps$elev)

  gotm$time$method <- 2
  gotm$time$start <- paste(date_range[1], "12:00:00")
  gotm$time$stop <- paste(date_range[2], "12:00:00")
  gotm$time$dt <- 3600

  gotm$light_extinction$method <- 7
  gotm$light_extinction$g2$method <- 0
  gotm$light_extinction$g2$constant_value <- round(1 / Kw, 2)

  gotm$grid$nlev <- nlev

  # keep the hyps table a manageable size
  if(nrow(hyps) > 20) {
    bathy <- hyps[, 1:2] |>
      dplyr::slice(c(seq(1,(nrow(hyps)-1), round(nrow(hyps) / 20)),
                     nrow(hyps)) )
  } else {
    bathy <- hyps[, 1:2]
  }

  if (ext_elev != 0){
    bathy.gotm <- data.frame(bathy) |>
      `names<-`(c("elev","area")) |>
      dplyr::mutate(elev = round(elev - max(elev),2)) |>
      bathy_extrap(0.75, ext_elev)
  } else {
    bathy.gotm <- data.frame(bathy)
  }


  z_diff <- round((min(bathy.gotm$elev) + init_depth))
  a0 <- round(approx(bathy.gotm$elev, bathy.gotm$area, z_diff)$y)
  if (!(z_diff %in% bathy.gotm$elev)) {
    bathy.gotm <- rbind(bathy.gotm, c(z_diff, a0))
  }


  bathy.gotm <- bathy.gotm |>
    dplyr::mutate(elev = round(elev - z_diff, 2)) |>
    dplyr::arrange(-elev)

  bathy.gotm <- rbind(c(round(nrow(bathy.gotm), 0),
                        round(ncol(bathy.gotm), 0)),
                      bathy.gotm) |>
    dplyr::mutate_all(as.character)


  # write the hypso file
  utils::write.table(bathy.gotm, file.path(path_gotm, "inputs/hypsograph.dat"),
                     sep = "\t", row.names = FALSE, quote = FALSE,
                     col.names = FALSE)

  gotm$location$hypsograph <- "inputs/hypsograph.dat"

  # Met ----
  gotm_met <- make_metGOTM(df_met = met, path_gotm, hum_type = hum_type)
  gotm_met_names <- names(gotm_met)[-c(1, 2)]

  for (m in 1:length(gotm_met_names)) {
    idx <- which(met_ref$std == gotm_met_names[m])
    gotm$surface$meteo[[met_ref$gotm[idx]]]$method <- 2
    gotm$surface$meteo[[met_ref$gotm[idx]]]$file <- "inputs/meteo.dat"
    gotm$surface$meteo[[met_ref$gotm[idx]]]$column <- which(gotm_met_names == met_ref$std[idx])
    gotm$surface$meteo[[met_ref$gotm[idx]]]$scale_factor <- 1 # ifelse(gotm_met_names[m] == "stn_press", 100, 1)
    gotm$surface$meteo[[met_ref$gotm[idx]]]$offset <- 0
    if (met_ref$gotm[idx] == "hum") {
      gotm$surface$meteo[[met_ref$gotm[idx]]]$type <- hum_type
    }
  }

  # Inflows ----
  gotm[["streams"]] <- NULL

  names.inf <- names(inf)

  if (length(names.inf) > 0) {
    for (f in 1:length(names.inf)) {

      df <- inf[[f]]

      if("NCS_ss2" %in% colnames(df)) {
        df$NCS_ss2 <- NULL
      }

      colnames(df) <- rename_modelvars(colnames(df), type_output = "gotm_wet")

      df <- df |>
        dplyr::mutate(time = "12:00:00",
                      flow = (flow * inf_factor) / 86400) |>
        dplyr::select(c("date", "time", everything()))

      utils::write.table(df[, c("date", "time", "flow")],
                         file.path(path_gotm, "inputs",
                                   paste0("inf_flow_", names.inf[f],".dat")),
                  row.names = FALSE, col.names = FALSE, quote = FALSE, na = "",
                  sep = "\t")

      ## write the temperature file
      utils::write.table(df[, c("date", "time", "temp")],
                         file.path(path_gotm, "inputs",
                                   paste0("inf_temp_", names.inf[f],".dat")),
                  row.names = FALSE, col.names = FALSE, quote = FALSE, na = "",
                  sep = "\t")
      ## write the salinity file
      utils::write.table(df[, c("date", "time", "salt")],
                         file.path(path_gotm, "inputs",
                                   paste0("inf_salt_", names.inf[f],".dat")),
                  row.names = FALSE, col.names = FALSE, quote = FALSE, na = "",
                  sep = "\t")

      inf_lst <- lapply(c("flow", "temp", "salt"), \(x) {
        list(method = 2, constant_value = 0,
             file = paste0("inputs/inf_", x, "_", names.inf[f],".dat"),
             column = 1, scale_factor = 1, offset = 0)
      })
      names(inf_lst) <- c("flow", "temp", "salt")

      if (use_bgc) {
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
                                     paste0("inf_chem_", names.inf[f],".dat")),
                           row.names = FALSE, col.names = FALSE, quote = FALSE, na = "",
                           sep = "\t")

        chem_lst <- lapply(3:ncol(df.chem), \(c) {
          list(method = 2, constant_value = 0,
               file = paste0("inputs/inf_chem_",names.inf[f],".dat"),
               column = c - 2, scale_factor = 1, offset = 0)
        })
        names(chem_lst) <- names(df.chem)[-c(1, 2)]

        gotm[["streams"]][[names.inf[f]]] <- c(list(method = 4, zu = 0, zl = -1),
                                          inf_lst, chem_lst)

      } else {
        gotm[["streams"]][[names.inf[f]]] <- c(list(method = 4, zu = 0, zl = -1),
                                          inf_lst)

      }
    }
  }

  if (use_bgc) {
    gotm[["fabm"]][["use"]] <- TRUE
  } else {
    gotm[["fabm"]][["use"]] <- FALSE
  }

  # Withdrawal ----
  if (length(outf) > 0) {

    make_wdrGOTM(outf = outf, path_gotm = path_gotm, outf_factor = outf_factor)

    # Update GOTM yaml file
    names.outf <- names(outf)
    for (w in 1:length(names.outf)) {
      gotm[["streams"]][[names.outf[w]]] <- list(method = 1, zu = 0, zl = -1,
                                                 flow = list(method = 2,
                                                             constant_value = 0,
                                                             file = paste0("inputs/outf_",
                                                                           names.outf[w],
                                                                           ".dat"),
                                                             column = 1,
                                                             scale_factor = 1,
                                                             offset = 0)
      )
    }

  }

  # Water balance settings
  gotm$water_balance_method <- 3
  gotm$mimic_3d$zeta$method <- 3

  write_yaml(gotm, file.path(path_gotm, "gotm.yaml"))
}
