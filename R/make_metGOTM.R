#' Write meteorological file for use in GOTM
#'
#' @param df_met 
#' @param path.gotm 
#' @param hum_type 
#' @param return_df 
#'
#' @importFrom dplyr mutate select all_of across
#' @importFrom utils write.table
#'
#' @return
#' @noRd
#'

make_metGOTM <- function(df_met, path.gotm, hum_type = 3, return_df = TRUE) {
  
  df_met <- df_met |>
    dplyr::mutate(time = "12:00:00")
  if (hum_type == 1) {
    col_sel <- c("Date","time","MET_wnduvu", "MET_wnduvv","MET_prsttn","MET_tmpair",
                 "MET_humrel","MET_cldcvr", "MET_radswd", "MET_pprain")
  } else if(hum_type == 3) {
    col_sel <- c("Date","time","MET_wnduvu", "MET_wnduvv","MET_prsttn","MET_tmpair",
                 "MET_tmpdew","MET_cldcvr", "MET_radswd", "MET_pprain")
  } 
  
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
  
  if (return_df) {
    return(met_main)
  }
}
