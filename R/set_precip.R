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
#' @param type character. Either "met" or "inflow". Default is
#' "inflow". If "met", precipitation is treated as a meteorological input
#' in mm. If "inflow", precipitation is converted to an inflow volume in m3. 
#' It is also possible to use the old argument values "precip_as_met" and 
#' "precip_as_inflow" for backward compatibility.
#'
#' @returns Aeme object with precipitation set as specified
#' @export
#' 
#' @importFrom dplyr mutate select left_join filter case_when
#' @importFrom zoo rollmean na.locf
#' @importFrom rlang arg_match
#'

set_precip <- function(aeme, type = c("inflow", "met", "precip_as_inflow",
                                      "precip_as_met")) {
  
  # 1. Match the argument (this allows "inf", "met", or the full old strings)
  type <- rlang::arg_match(type)
  
  # 2. Map old names to new names for internal consistency
  if (type == "precip_as_inflow") type <- "inflow"
  if (type == "precip_as_met") type <- "met"
  
  aeme <- check_aeme(aeme)
  met <- get_met(aeme)
  lke <- get_lake(aeme)
  lake_area <- lke[["area"]]
  curr_status <- precip_status(aeme)
  if (type == curr_status) {
    message("Precip already set as ", type)
    return(aeme)
  }
  
  hyps <- get_hypsograph(aeme)
  if (is.null(lake_area)) {
    lake_area <- hyps |> 
      dplyr::filter(depth == 0) |>
      dplyr::pull(area)
  }
  
  precip <- met |> 
    dplyr::mutate(precip_m = MET_pprain + MET_ppsnow, 
                  precip_mm = precip_m * 1000) |> 
    dplyr::select(Date, precip_mm, precip_m)
  
  if (type == "inflow") {
    
    # Check if water level observations are present
    obs <- get_obs(aeme, var_sim = "LKE_lvlwtr")
    if (nrow(obs) > 0) {
      full_date <- met |> 
        dplyr::select(Date)
      obs <- obs |> 
        dplyr::select(Date, value) |> 
        dplyr::right_join(full_date, by = "Date") |>
        # Fill NAs in value
        dplyr::mutate(value = zoo::na.locf(value, na.rm = FALSE)) |> 
        dplyr::mutate(area = area_from_level(h = value, hyps = hyps))
      
      precip <- precip |> 
        # dplyr::filter(Date >= date_range[1] & Date <= date_range[2]) |> 
        dplyr::left_join(obs, by = "Date") |> 
        dplyr::mutate(precip_vol = precip_m * area) |> 
        dplyr::select(Date, precip_mm, precip_m, area, precip_vol)
      # Fill NA
      
    } else {
      precip <- precip |>  #((met[["MET_pprain"]] + met[["MET_ppsnow"]]) / 1000) * lake_area
        dplyr::mutate(precip_vol = precip_m * lake_area)
    }
    
    precip_vol <- precip[["precip_vol"]]
    
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
  } else if (type == "met") {
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
          message(paste0("Inflows: 'precip_as_met' Adding preceding zeros (n=", n_add, 
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
  } else {
    cli::cli_abort("Invalid type specified. Must be either 'inflow' or 'met'.")
  }
  return(aeme)
}
