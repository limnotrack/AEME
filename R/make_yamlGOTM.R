#' Modify the GOTM yaml file for ensemble simulation
#'
#' @inheritParams delagrangify
#' @inheritParams initialiseGLM
#' @inheritParams initialiseGOTM
#' @inheritParams build_dycd
#' @inheritParams build_aeme
#'
#' @return write updated GOTM yaml file in GOTM directory.
#' @noRd

make_yamlGOTM <- function(gotm, lakename, date_range, hyps, lat, lon, nlev, met,
                          inf, outf, init_depth, path_gotm, ext_elev,
                          outf_factor, inf_factor, Kw, use_bgc, hum_type = 1,
                          est_swr_hr = TRUE) {

  met_ref <- data.frame(gotm = c("u10", "v10", "airp", "airt", "hum", "hum",
                                 "cloud", "swr", "precip"),
                        std = c("MET_wnduvu", "MET_wnduvv", "MET_prsttn",
                                "MET_tmpair", "MET_tmpdew", "MET_humrel",
                                "MET_cldcvr", "MET_radswd", "precip"))


  gotm$location$name <- lakename
  gotm$location$latitude <- lat
  gotm$location$longitude <- lon
  gotm$location$depth <- init_depth

  gotm$time$method <- 2
  gotm$time$start <- paste(date_range[1], "00:00:00")
  gotm$time$stop <- paste(date_range[2], "00:00:00")
  gotm$time$dt <- 3600

  gotm$light_extinction$method <- 7
  gotm$light_extinction$g2$method <- 0
  gotm$light_extinction$g2$constant_value <- round(1 / Kw, 2)

  # gotm$grid$nlev <- nlev

  # keep the hyps table a manageable size
  if(nrow(hyps) > 20) {
    bathy <- hyps |>
      dplyr::slice(c(seq(1,(nrow(hyps)-1), round(nrow(hyps) / 20)),
                     nrow(hyps)) )
  } else {
    bathy <- hyps
  }

  if (ext_elev != 0){
    bathy.gotm <- data.frame(bathy) |>
      dplyr::mutate(elev = round(elev - (min(elev) + init_depth),2)) |>
      bathy_extrap(0.75, ext_elev)
  } else {
    bathy.gotm <- data.frame(bathy)
  }
  bathy.gotm <- bathy.gotm |>
    dplyr::mutate(elev = round(elev, 2))


  z_diff <- round((min(bathy.gotm$elev) + init_depth), 2)
  a0 <- round(approx(bathy.gotm$elev, bathy.gotm$area, z_diff)$y)
  if (!(z_diff %in% bathy.gotm$elev)) {
    bathy.gotm <- rbind(bathy.gotm, c(z_diff, a0, 0))
  }


  bathy.gotm <- bathy.gotm |>
    dplyr::mutate(elev = round(elev - z_diff, 2)) |>
    dplyr::arrange(-elev)  |>
    dplyr::select(elev, area)

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
  met_cols <- make_metGOTM(df_met = met, path_gotm, hum_type = hum_type,
                           lat = lat, lon = lon,
                           est_swr_hr = est_swr_hr)
  gotm_met_names <- met_cols[-c(1, 2)]

  for (m in 1:nrow(met_ref)) {

    if (met_ref$gotm[m] == "swr" | met_ref$std[m] %in% gotm_met_names) {
      # idx <- which(met_ref$std == gotm_met_names[m])
      if (met_ref$gotm[m] == "swr") {
        file <- "inputs/meteo_swr.dat"
        column <- 1
      } else {
        file <- "inputs/meteo.dat"
        column <- which(gotm_met_names == met_ref$std[m])
      }
      gotm$surface$meteo[[met_ref$gotm[m]]]$method <- 2
      gotm$surface$meteo[[met_ref$gotm[m]]]$file <- file
      gotm$surface$meteo[[met_ref$gotm[m]]]$column <- column
      gotm$surface$meteo[[met_ref$gotm[m]]]$scale_factor <- 1
      gotm$surface$meteo[[met_ref$gotm[m]]]$offset <- 0
      if (met_ref$gotm[m] == "hum") {
        gotm$surface$meteo[[met_ref$gotm[m]]]$type <- hum_type
      }
    }
  }

  # Inflows ----
  gotm[["streams"]] <- NULL
  gotm <- make_infGOTM(inf_list = inf, path_gotm = path_gotm, gotm = gotm,
                       inf_factor = inf_factor, update_gotm = TRUE,
                       use_bgc = use_bgc)

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

  gotm
}
