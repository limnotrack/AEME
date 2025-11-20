#' Convert netCDF output to a standardised list
#'
#' @inheritParams build_aeme
#' @param vars_sim vector; of variables to extract
#' @param incl_fluxes logical; whether to include flux variables (if available)
#' @param output_hour numeric; hour of day to extract. Default is 0 (midnight).
#'
#' @return list of data.frames for each model containing the variables
#' specified in \code{vars_sim}
#' @noRd
#'
#' @importFrom ncdf4 nc_open nc_close ncvar_get ncatt_get
#' @importFrom utils data
#' @importFrom dplyr filter mutate pull select bind_rows
#' @importFrom withr local_locale local_timezone
#' @importFrom lubridate hour
#' @importFrom rLakeAnalyzer thermo.depth center.buoyancy meta.depths

read_model_nc <- function(aeme, model, path, vars_sim, incl_fluxes = TRUE,
                          output_hour = 0, remove_spin_up = FALSE) {
  
  model <- check_model(model)
  aeme  <- check_aeme(aeme)
  path  <- check_path(path, must_exist = TRUE)
  lake_dir <- get_lake_dir(aeme, path)
  date_index <- get_date_index(aeme = aeme, model = model,
                               remove_spin_up = remove_spin_up)[[model]]
  
  out <- read_model_outputs(
    lake_dir   = lake_dir,
    model      = model,
    vars_sim   = vars_sim,
    incl_fluxes = incl_fluxes,
    date_index = date_index,
    output_hour = output_hour
  )
  
  return(out)
}

#' Get netCDF file path for model
#' @param lake_dir character; lake directory
#' @param model character; model name
#' @return character; path to netCDF file
#' @noRd
get_nc_path <- function(lake_dir, model) {
  if (model == "dy_cd") {
    file.path(lake_dir, model, "DYsim.nc")
  } else {
    file.path(lake_dir, model, "output", "output.nc")
  }
}

#' Extract model time information from netCDF
#' @param nc netCDF object
#' @param var character; name of time variable
#' @param units_prefix character; prefix to remove from time units attribute
#' @param output_hour numeric; hour of day to extract
#' @return list with dates and indices
#' @noRd
extract_model_time <- function(nc, var = "time", units_prefix, output_hour) {
  times <- ncdf4::ncvar_get(nc, var)
  if (!length(times)) return(NULL)
  
  origin <- ncdf4::ncatt_get(nc, var, "units")$value |>
    gsub(units_prefix, "", x = _) |>
    as.POSIXct()
  
  dates <- as.POSIXct(times + origin)
  idx   <- which(lubridate::hour(dates) == output_hour)
  
  if (!length(idx))
    stop("No output at hour ", output_hour)
  
  list(
    dates = as.Date(dates[idx]),
    idx   = idx
  )
}
