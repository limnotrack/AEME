#' Set precipitation as either meteorological input or inflow
#' 
#' This function allows you to specify whether precipitation should be treated as 
#' a meteorological input (in mm) or as an inflow volume (in m3) to the lake.
#' When set as an inflow, the function calculates the inflow volume based on the
#' lake surface area and the precipitation amount. Conversely, when set as a
#' meteorological input, it converts the inflow volume back to precipitation depth.
#' This is useful for hydrodynamic modeling where the representation of precipitation
#' can impact the simulation results, particularly in terms of water balance and
#' thermal structure.
#'
#' @inheritParams build_aeme
#' @param type character. Either "precip_as_met" or "precip_as_inflow". Default is
#' "precip_as_inflow". If "precip_as_met", precipitation is treated as a meteorological input
#' in mm. If "precip_as_inflow", precipitation is converted to an inflow volume in m3.
#'
#' @returns Aeme object with precipitation set as specified
#' @export
#'

set_precip <- function(aeme, type = "precip_as_inflow") {
  
  aeme <- check_aeme(aeme)
  met <- get_met(aeme)
  lke <- get_lake(aeme)
  lake_area <- lke[["area"]]
  curr_status <- precip_status(aeme)
  if (type == curr_status) {
    message("Precip already set as ", type)
    return(aeme)
  }
  
  if (is.null(lake_area)) {
    lake_area <- get_hypsograph(aeme) |> 
      dplyr::filter(depth == 0) |>
      dplyr::pull(area)
  }
  
  if (type == "precip_as_inflow") {
    # Convert precip to inflow volume (m3)
    precip_vol <- ((met[["MET_pprain"]] + met[["MET_ppsnow"]]) / 1000) * lake_area
    
    # Set precip to 0
    met[["MET_pprain"]] <- 0
    met[["MET_ppsnow"]] <- 0
    
    aeme <- add_met(aeme = aeme, met = met)
    
    # Convert to dataframe for inflows
    inf_precip <- data.frame(Date = met$Date, HYD_flow = precip_vol, 
                             HYD_temp = zoo::rollmean(met$MET_tmpair, 3, 
                                                      fill = "extend", 
                                                      align = "right"),
                             CHM_salt = 0, PHS_frp = 0, PHS_dop = 0, 
                             PHS_pop = 0, PHS_pip = 0, NIT_amm = 0,
                             NIT_nit = 0, NIT_don = 0,
                             inflow_id = "precip"
    ) |> 
      dplyr::filter(!is.na(HYD_temp)) |> 
      dplyr::mutate(HYD_temp = dplyr::case_when(
        HYD_temp < 4 ~ 4, .default = HYD_temp
      ))
    
    aeme <- add_inflow(aeme = aeme, inflow = inf_precip)
  } else if (type == "precip_as_met") {
    inf <- get_inflows(aeme)
    inf_names <- names(inf)
    if ("precip" %in% inf_names) {
      inf_vol <- inf[["precip"]][["HYD_flow"]]
      # Convert inflow volume (m3) to precip (mm)
      precip_mm <- (inf_vol / lake_area) * 1000
      if (length(precip_mm) != nrow(met)) {
        diff_len <- length(precip_mm) - nrow(met)
        # Add preceding zeros if needed
        if (diff_len >= -5 & diff_len < 0) {
          n_add <- nrow(met) - length(precip_mm)
          message(paste0("Adding preceding zeros (n=", n_add, 
                         ") to match meteorological data length"))
          precip_mm <- c(rep(0, n_add), precip_mm)
        } else {
          stop("Inflow and meteorological data have incompatible lengths")
        }
      }
      met[["MET_pprain"]] <- precip_mm
      met[["MET_ppsnow"]] <- 0
      aeme <- add_met(aeme = aeme, met = met)
      aeme <- remove_inflow(aeme = aeme, inflow_id = "precip")
    } else {
      stop("No 'precip' inflow found to convert to meteorological input")
    }
  }
  return(aeme)
}
