#' Read GOTM flux output
#'
#' @inheritParams read_glm_output
#'
#' @returns List with GOTM flux output variables
#' @export
#' 
#' @importFrom ncdf4 ncvar_get nc_close
#' @importFrom cli cli_abort
#'
read_gotm_flux_output <- function(nc = NULL, file, dates = NULL, 
                                  date_index = NULL) {
  
  if (is.null(nc)) {
    nc <- open_nc_safe(file = file, model = "gotm_wet")
    on.exit(ncdf4::nc_close(nc))
    if (nc$error) {
      cli::cli_abort("Could not open netCDF file: {.file {file}}")
    }
  }
  
  out_steps <- ncdf4::ncvar_get(nc, "time")
  if (length(out_steps) == 0) {
    return(NULL)
  }
  date_start <- ncdf4::ncatt_get(nc, "time", "units")$value |>
    gsub("seconds since ", "", x = _) |>
    as.POSIXct()
  time_vec <- ncdf4::ncvar_get(nc, "time")
  gotm_dates <- as.POSIXct(time_vec + date_start) |> 
    as.Date()
  if (is.null(date_index)) {
    if (!is.null(dates)) {
      date_index <- which(gotm_dates %in% dates)
      if (length(date_index) == 0) {
        cli::cli_abort("No output for GOTM at specified dates")
      }
    } else {
      date_index <- seq_along(gotm_dates)
    }
  }

  t_start <- date_index[1]
  start_1d <- c(1, 1, t_start)
  start_2d <- c(1, 1, 1, t_start)
  
  lyr_h2 <- ncdf4::ncvar_get(nc, "h")[, date_index]
  Af <- ncdf4::ncvar_get(nc, "Af")[, date_index]
  V <- sapply(seq_len(ncol(Af)), \(i) {
    sum(lyr_h2[, i] * Af[, i])
  })
  Qe <- ncdf4::ncvar_get(nc, "qe")[date_index]
  Qh <- ncdf4::ncvar_get(nc, "qh")[date_index]
  Qlw <- ncdf4::ncvar_get(nc, "ql")[date_index]
  Qsw <- ncdf4::ncvar_get(nc, "I_0")[date_index]
  evap_flux <- abs(ncdf4::ncvar_get(nc, "evap")[date_index])
  EVAP <- evap_flux * 86400 # m/s -> m/day
  A0 <- Af |>
    apply(2, max)
  evap_vol <- EVAP * A0
  
  flow_vars <- names(nc$var)[grepl("Q_", names(nc$var))]
  inflow_vars <- flow_vars[!grepl("outflow|wbal", flow_vars)]
  outflow_vars <- flow_vars[grepl("outflow|wbal", flow_vars)]
  if (length(inflow_vars) >= 1) {
    inflow <- sapply(seq_along(inflow_vars), \(x) {
      (ncdf4::ncvar_get(nc, inflow_vars[x])[date_index] * 86400) # / A0
    }) |>
      apply(1, sum)
  } else {
    inflow <- A0 * 0
  }
  if (length(outflow_vars) >= 1) {
    outflow <- sapply(seq_along(outflow_vars), \(x) {
      -1 * (ncdf4::ncvar_get(nc, outflow_vars[x])[date_index] * 86400) # / A0
    }) |>
      apply(1, sum)
  } else {
    outflow <- A0 * 0
  }
  precip <- ncdf4::ncvar_get(nc, "precip")[date_index] * 86400
  precip_vol <- precip * A0
  Ts <- ncdf4::ncvar_get(nc, "temp")[, date_index]
  Ts <- Ts[nrow(Ts), ]
  MET_tmpair <- ncdf4::ncvar_get(nc, "airt")
  
  out_list <- list(
    LKE_V = as.vector(V),
    LKE_A0 = as.vector(A0),
    LKE_evprte = as.vector(EVAP),
    LKE_evpflx = as.vector(evap_flux),
    LKE_evpvol = as.vector(evap_vol),
    LKE_Qe = as.vector(Qe),
    LKE_Qh = as.vector(Qh),
    LKE_Qlw = as.vector(Qlw),
    LKE_Qsw = as.vector(Qsw),
    # LKE_evapvol = as.vector(evap_vol),
    LKE_precip = as.vector(precip),
    LKE_pcpvol = as.vector(precip_vol),
    LKE_inflow = as.vector(inflow),
    LKE_outflow = as.vector(outflow),
    HYD_surft = as.vector(Ts),
    MET_tmpair = as.vector(MET_tmpair),
    HYD_atdiff = as.vector(Ts - MET_tmpair)
  )
  return(out_list)
}
