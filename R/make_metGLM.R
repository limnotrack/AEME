#' Make a met file for GLM
#'
#' @param obs_met
#' @param path_glm
#' @param infRain
#' @param use_lw
#'
#' @noRd
#' @importFrom stringr str_pad
#' @importFrom dplyr select mutate across
#'
make_metGLM <-  function(obs_met, path_glm = "", infRain = FALSE,
                         use_lw = FALSE) {

  if (use_lw) {
    col.order <- c("Date", "MET_radswd", "MET_radlwd", "MET_tmpair",
                   "MET_humrel", "MET_wndspd", "MET_pprain", "MET_ppsnow",
                   "MET_prsttn")
    col.names <- c("time      ",
                  c("ShortWave", "LongWave", "AirTemp", "RelHum", "WindSpeed",
                    "Rain", "Snow", "AirPres") |>
                    stringr::str_pad(width = 12, side = c("left"), pad = " "))
  } else {
    col.order <- c("Date", "MET_radswd", "MET_cldcvr", "MET_tmpair",
                   "MET_humrel", "MET_wndspd", "MET_pprain", "MET_ppsnow",
                   "MET_prsttn")
    col.names <- c("time      ",
                  c("ShortWave", "Cloud", "AirTemp", "RelHum", "WindSpeed",
                    "Rain", "Snow", "AirPres") |>
                    stringr::str_pad(width = 12, side = c("left"), pad = " "))
  }


  # process the obsMet into DY format
  metVals <- obs_met |>
    dplyr::select(all_of(col.order)) |>
    dplyr::mutate(MET_prsttn = MET_prsttn / 100)
  metVals <- metVals |>
    # standardise formats
    dplyr::mutate(dplyr::across(2:6, \(x) round(x, digits = 3)),
                  dplyr::across(2:6, \(x) format(x, nsmall = 3, width = 12)),
                  dplyr::across(7:ncol(metVals), \(x) round(x, digits = 5)),
                  dplyr::across(7:ncol(metVals), \(x) format(x, nsmall = 5,
                                                       width = 12)))

  # set rain to zero, if specified
  if (infRain == TRUE) {
    metVals$rain_m <- 0
    print("Rainfall set to zero (prescribed via .inf instead).")
  }

  #-------- make the file!
  f <- file(file.path(path_glm, "bcs/meteo_glm.csv"), "w")

  writeLines(paste0(col.names, collapse = ","), f)
  write.table(metVals, f, sep = ",", quote = FALSE, row.names = FALSE,
              col.names = FALSE)

  close(f)

}
