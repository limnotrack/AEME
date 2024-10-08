#' Write meteorological file for use in GOTM
#'
#' @param df_met data.frame of meteorological variables.
#' @param path.gotm filepath; to GOTM directory.
#' @param hum_type numeric; humidity metric [1=relative humidity (%),
#' 2=wet-bulb temperature, 3=dew point temperature, 4=specific humidity (kg/kg);
#'  default=3]
#' @param return_df Logical; return meteorological dataframe
#'
#' @importFrom dplyr mutate select all_of across
#' @importFrom utils write.table
#'
#' @return writes met file to GOTM directory or returns dataframe with GOTM met
#' variables.
#' @noRd

make_metGOTM <- function(df_met, path.gotm, hum_type = 3, lat, lon,
                         est_swr_hr = TRUE, return_colname = TRUE) {

  df_met <- df_met |>
    dplyr::mutate(time = "12:00:00") |>
    dplyr::mutate(MET_pprain = MET_pprain / 1000, # convert to m
                  MET_ppsnow = MET_ppsnow / 1000) #, # convert to m
                  # MET_prsttn = MET_prsttn * 100) # convert to Pa
  if (hum_type == 1) {
    col_sel <- c("Date", "time", "MET_wnduvu", "MET_wnduvv", "MET_prsttn",
                 "MET_tmpair", "MET_humrel", "MET_cldcvr", "MET_pprain")
  } else if(hum_type == 3) {
    col_sel <- c("Date", "time", "MET_wnduvu", "MET_wnduvv","MET_prsttn",
                 "MET_tmpair", "MET_tmpdew", "MET_cldcvr", "MET_pprain")
  }

  # SWR met file
  if (est_swr_hr) {
    met_swr <- df_met |>
      dplyr::select(Date, MET_radswd)

    met_swr <- estimate_hourly_swr(met = met_swr, lat = lat, lon = lon)

    met_swr <- met_swr |>
      dplyr::mutate(dplyr::across(2:ncol(met_swr), \(x) signif(x, 6)),
                    dplyr::across(2:ncol(met_swr), \(x) format(x, nsmall = 4,
                                                               width = 12)))
  } else {
    met_swr <- df_met |>
      dplyr::select(Date, time, MET_radswd)

    met_swr <- met_swr |>
      dplyr::mutate(dplyr::across(3:ncol(met_swr), \(x) signif(x, 6)),
                    dplyr::across(3:ncol(met_swr), \(x) format(x, nsmall = 4,
                                                               width = 12)))
  }


  utils::write.table(met_swr, file.path(path.gotm, "inputs", "meteo_swr.dat"),
                     row.names = FALSE, col.names = FALSE, quote = FALSE,
                     na = "", sep = "\t")

  # Main met file
  met_main <- df_met |>
    dplyr::select(dplyr::all_of(col_sel)) |>
    dplyr::mutate(precip = MET_pprain / (86400)) |>
    dplyr::select(-MET_pprain)

  met_main <- met_main |>
    dplyr::mutate(dplyr::across(3:ncol(met_main), \(x) signif(x, 6)),
                  dplyr::across(3:ncol(met_main), \(x) format(x, nsmall = 4,
                                                              width = 12)))

  utils::write.table(met_main, file.path(path.gotm, "inputs", "meteo.dat"),
                     row.names = FALSE, col.names = FALSE, quote = FALSE,
                     na = "", sep = "\t")

  if (return_colname) {
    return(names(met_main))
  }
}
