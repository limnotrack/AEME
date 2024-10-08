#' Make a .met (meteorology) file for DY(CD) simulation
#'
#' @param lakename name of lake being simulated
#' @param info extra text to be printed in file header (base = 'DYRESM meterological inputs file for lake <lake>')
#' @param obsMet data frame of meteorological data in DY order with standard dates (YYYY-MM-DD)
#' @param infRain TRUE = set .met rain to zero (to be put in as inflow instead)
#' @param verDY DYRESM version
#' @param z_max long-term average max depth of lake
#' @param metHeight height of met station above lake surface
#' @param wndType 0 = fixed height, 1 = floating
#' @param filePath path to write the complete .met file
#' @keywords inputs
#'
#' @importFrom dplyr select mutate across
#' @importFrom lubridate year
#' @importFrom utils write.table
#'
#' @noRd
#'



make_DYmet <-  function(lakename = "unknown",
                        info = "",
                        verDY = 3.1,
                        obsMet,
                        infRain = FALSE,
                        wndType = 0,
                        metHeight = 10,
                        z_max,
                        filePath = "",
                        use_lw) {

  if (use_lw) {
    col.order <- c("Date", "MET_radswd", "MET_radlwd", "MET_tmpair", "MET_prvapr", "MET_wndspd", "MET_pprain")
    col.names <- c("Date   ",
                   c("solar_Wm^2", "lwave_Wm^2", "tempture_C", "vapour_hPa", "wind_ms^-1", "rain_m") |>
                     stringr::str_pad(width = 12, side=c('left'), pad=' ') )
    lw_type <- "INCIDENT_LW"
  } else {
    col.order <- c("Date", "MET_radswd", "MET_cldcvr", "MET_tmpair", "MET_prvapr", "MET_wndspd", "MET_pprain")
    col.names <- c("Date   ",
                   c("solar_Wm^2", "cloud_0to1", "tempture_C", "vapour_hPa", "wind_ms^-1", "rain_m") |>
                     stringr::str_pad(width = 12, side=c('left'), pad=' ') )
    lw_type <- "CLOUD_COVER"
  }


  # process the obsMet into DY format
  metVals <- obsMet |>
    dplyr::mutate(MET_pprain = MET_pprain / 1000,
                  MET_ppsnow = MET_ppsnow / 1000) |> # convert to m
    dplyr::select(dplyr::all_of(col.order))
  metVals <- metVals |>
    dplyr::mutate(Date = paste0(lubridate::year(Date), strftime(Date, format = "%j")),
                  dplyr::across(2:6, \(x) round(x, digits = 3)),
                  dplyr::across(2:6, \(x) format(x, nsmall = 3, width = 12)),
                  dplyr::across(7:ncol(metVals), \(x) round(x, digits = 5)),
                  dplyr::across(7:ncol(metVals), \(x) format(x, nsmall = 5,
                                                       width = 12))) |>
    `names<-`(col.names)

  #  adding snow if required for DY version
  if (verDY >= 3.2) {
    metVals$PpSnow = 0
  }



  # set rain to zero, if specified
  if (infRain == TRUE){
    metVals$PpRain <- 0
    info <- paste0(info,". Rainfall set to zero (prescribed via .inf instead).")
  }

  # set wind sensor type and height
  if (wndType == 0) {
    wndType   <- "FIXED_HT"
    metHeight <- z_max + metHeight
  } else if (wndType == 1) {
    wndType <- "FLOATING"
  } else {
    print("!Wind sensor type (0 = fixed, 1 = floating) is unknown!")
  }

  #-------- make the file! ---------

  # open file connection
  f <- file(paste0(filePath,"/",lakename,".met"),"w")

  # add headers
  writeLines(c(paste0("<#3>"),
               paste0("DYRESM meterological inputs file for lake ",lakename,". ",info),
               paste0("86400		   # met data input time step (seconds)"),
               paste0(lw_type, "	   # longwave radiation type (NETT_LW, INCIDENT_LW, CLOUD_COVER)"),
               paste0(wndType,"  ", round(metHeight, 2),"   # sensor type (FLOATING, FIXED_HT), height in metres (above water surface, above lake bottom)"),
               paste0(col.names, collapse = "\t")), f)

  utils::write.table(metVals, f, sep = "\t", quote = FALSE, row.names = FALSE,
              col.names = FALSE)

  close(f)

}
