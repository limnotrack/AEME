#' Read DYRESM output
#'
#' @inheritParams read_glm_output
#'
#' @returns List with AEME output variables
#' @export
#'
#' @importFrom ncdf4 ncvar_get ncatt_get nc_close
#' @importFrom lubridate hour
#' @importFrom dplyr filter mutate select pull
read_dy_output <- function(nc = NULL, vars_sim = NULL, depths = NULL,
                           dates = NULL, date_index = NULL, incl_fluxes = FALSE, 
                           output_hour = 0, file) {
  
  # Set timezone
  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")
  
  if (is.null(nc)) {
    nc <- open_nc_safe(file, model = "dy_cd")
    on.exit(ncdf4::nc_close(nc))
  }
  out_list <- list()
  
  # dyresm outputs initial profiles as first col
  if (!("dyresmTime" %in% names(nc$var))) {
    out <- empty_model_output(reason = "Empty time dimension")
    return(out)
  }
  dy_time <- ncdf4::ncvar_get(nc, "dyresmTime")
  dy_time[dy_time > 9.9e36] <- NA
  dy_dates <- as.POSIXct((dy_time - 2415018.5) *
                           86400, origin = "1899-12-30")
  idx <- which(lubridate::hour(dy_dates) == output_hour)
  if (length(idx) == 0) stop("No output for DYRESM at ", output_hour, " hour")
  dy_dates <- dy_dates |> as.Date()
  valid_dates <- dy_dates[!is.na(as.numeric(dy_dates))]
  if (is.null(date_index)) {
    if (!is.null(dates)) {
      date_index <- which(dy_dates %in% dates)
      if (length(date_index) == 0) {
        cli::cli_abort("No output for DYRESM at specified dates")
      }
    } else {
      date_index <- seq_along(dy_dates)
    }
  }
  if (length(valid_dates) < length(date_index)) {
    cli::cli_alert_warning("date_index exceeds available DYRESM output dates. 
                          Returning empty output.")
    out <- empty_model_output(
      reason = "date_index exceeds available DYRESM output dates"
    )
    return(out)
  }
  dates <- dy_dates[date_index]
  out_list[["Date"]] <- dates
  
  # mod_layers are elevation from bottom, last row is bottom
  # ncdf4::ncatt_get(nc, "dyresmLAYER_HTS_Var")
  # var <- ncdf4::ncvar_get(nc, paste0('dyresm', "TEMPTURE",'_Var'))
  mod_layers <- ncdf4::ncvar_get(nc, "dyresmLAYER_HTS_Var")[, date_index]
  if (!is.matrix(mod_layers)) {
    cli_inform_safe("Error reading DYRESM layers, potentially due to water level
                   fluctuations.\nReturning NULL...")
    return(NULL)
  }
  
  lake_level <- apply(mod_layers, 2, \(x) max(x, na.rm = TRUE))
  midpoints <- apply(mod_layers, 2, \(x) {
    x <- rev(x)
    res <- x - diff(c(0, x)) / 2
    rev(res)
  })
  L_mat <- matrix(lake_level, nrow = nrow(midpoints), ncol = length(lake_level),
                 byrow = TRUE)
  midpoints <- L_mat - midpoints
  
  if (is.null(depths)) {
    max_depth <- max(lake_level, na.rm = TRUE)
    data("model_layer_structure", package = "AEME", envir = environment())
    depth_fraction <- model_layer_structure |> 
      dplyr::filter(z < max_depth) |> 
      dplyr::mutate(deps = z / max_depth) |> 
      dplyr::pull(deps) |> 
      matrix(ncol = 1)
    depth_mat <- depth_fraction %*% t(lake_level)
    out_depths <- round(depth_mat, 3)
  } else {
    out_depths <- matrix(rep(depths, length(dates)),
                         nrow = length(depths),
                         ncol = length(dates))
  }
  out_list[["LKE_lvlwtr"]] <- lake_level
  out_list[["LKE_depths"]] <- out_depths
  
  if (!is.null(vars_sim)) {
    model_vars <- get_model_vars(vars_sim = vars_sim, model = "dy_cd")
    model_vars_vec <- format_model_vars_vec(vars_sim = vars_sim, 
                                            model = "dy_cd")
    
    nc_vars <- names(nc$var)
    vars_chk <- data.frame(vars = model_vars_vec,
                           present = model_vars_vec %in% nc_vars)
    
    out_vars <- lapply(model_vars_vec, \(v) {
      if(vars_chk$present[vars_chk$vars == v] == FALSE) {
        return(NULL)
      }
      var <- ncdf4::ncvar_get(nc, v)[, date_index]
      interp_static_grid(var = var,
                         midpoints = midpoints,
                         out_depths = out_depths)
    })
    out_list <- c(out_list, out_vars)
  }
  
  if (incl_fluxes) {
    H <- ncdf4::ncvar_get(nc, "morph_HEIGHT")
    A <- ncdf4::ncvar_get(nc, "morph_AREA")
    elev <- ncdf4::ncvar_get(nc, "morph_ELEV")
    init_H <- ncdf4::ncvar_get(nc, "initprofHeight")[2] + min(H)
    
    MET_wndspd <- ncdf4::ncvar_get(nc, "met_Uwind")[idx]
    MET_prvapr <- ncdf4::ncvar_get(nc, "met_Pvapour")[idx]
    MET_tmpair <- ncdf4::ncvar_get(nc, "met_Tair")[idx]
    out_list[["HYD_surft"]] <- ncdf4::ncvar_get(nc, "dyresmTEMPTURE_Var")[, idx] |>
      apply(2, \(x) {
        x[!is.na(x)][1]
      })
    
    # Saturated vapour pressure - Magnus-Tetens formula (TVA 1972, eqn 4.1)
    es <- exp(2.3026 * (((7.5 * out_list[["HYD_surft"]]) /
                           (out_list[["HYD_surft"]] + 237.3) + 0.7858)))
    #evaporative heat flux
    out_list[["LKE_Qe"]] <- ((0.622 / 981.9) *         #constant/mean station pressure
             0.0013 *               #latent heat transfer coefficient
             1.168 *                #density of air
             2453000 *              #latent heat of evaporation of water
             MET_wndspd *           #wind speed in m/s
             (MET_prvapr - es))
    out_list[["LKE_Qe"]][out_list[["LKE_Qe"]] > 0] <- 0 # evaporation can't be negative
    
    # Conductive/sensible heat gain only affects the top layer.
    # Q_sensibleheat = -CH * rho_air * cp_air * WindSp * (Lake[surfLayer].Temp - MetData.AirTemp);
    # rho_air <- atm_density(MET_tmpair, 101325)
    Q_lw_in <- ncdf4::ncvar_get(nc, "met_LW_related")[date_index]
    if (all(Q_lw_in <= 1)) {
      CloudCover <- Q_lw_in
      # eps_star <- (1.0 + 0.275 * CloudCover) * (1.0 - 0.261 * exp(-0.000777 * MET_tmpair^2.0))
      # Q_lw_in <- (1 - 0.03) * eps_star *
      #   5.678e-8  * # Stefan_Boltzman constant
      #   (273.15 + Ts)^4.0 # water surface temperature in Kelvin
      
      # DYRESM Science Manual
      Q_lw_in <- (1 - 0.03) * # albedo for long wave radiation, constant = 0.03 (Henderson-Sellers 1986).
        (1 + 0.17 * CloudCover^2) *
        (9.37e-6 * (MET_tmpair + 273.15)^2) * # Swinbank (1963)
        5.6697e-8 * # Stefan-Boltzmann constant
        (MET_tmpair + 273.15)^4
      
    }
    out_list[["LKE_Qh"]] <- 1.3e-3 * # sensible heat transfer coefficient for wind speed at 10 m reference height above the water surface
      1.168 * # density of air
      1003.0 *    # cp_air Specific heat of air
      MET_wndspd *
      (MET_tmpair - out_list[["HYD_surft"]])
    Q_lw_out <- -5.678e-8  * # Stefan_Boltzman constant
      0.985 * # emissivity of water
      (273.15 + out_list[["HYD_surft"]])^4.0 # water surface temperature in Kelvin
    out_list[["LKE_Qlw"]] <- Q_lw_out + Q_lw_in
    
    # Qlw <- NA
    out_list[["LKE_Qsw"]] <- ncdf4::ncvar_get(nc, "met_SW")[idx]
    EVAP <- ncdf4::ncvar_get(nc, "dyresmEVAP_DAILY_Var")[idx]
    
    inflow_vars <- names(nc$var)[grepl("stream", names(nc$var)) &
                                   grepl("VOL", names(nc$var)) ]
    if (length(inflow_vars) >= 1) {
      out_list[["LKE_inflow"]] <- sapply(seq_along(inflow_vars), \(x) {
        ncdf4::ncvar_get(nc, inflow_vars[x])[idx]
      }) |>
        apply(1, sum)
    } else {
      inflow <- Ts * 0
    }
    outflow_vars <- names(nc$var)[grepl("withdrawal", names(nc$var)) &
                                    grepl("VOL", names(nc$var))]
    if (length(outflow_vars) >= 1) {
      outflow <- sapply(seq_along(outflow_vars), \(x) {
        ncdf4::ncvar_get(nc, outflow_vars[x])[idx]
      }) |>
        apply(1, sum)
    } else {
      outflow <- Ts * 0
    }
    
    out_list[["LKE_outflow"]] <- outflow +
      ncdf4::ncvar_get(nc, "overflow_VOL_Var")[idx]
    precip <- ncdf4::ncvar_get(nc, "met_RAIN")[idx]
    
    out_list[["LKE_A0"]] <- sapply(1:length(lake_level), function(d) approx((H - min(H)), A,
                                                     xout = lake_level[d])$y)
    dz <- 0.01
    adj_dep <- min(H)
    out_list[["LKE_V"]] <- sapply(1:length(lake_level), function(d) {
      if(is.na(lake_level[d]) | is.infinite(lake_level[d])) return(NA)
      layerD <- (seq(dz, (lake_level[d] - dz), dz)) + adj_dep
      layerA <- approx(H, A, layerD)$y
      sum((layerA) * dz)
    })
    # inflow <- inflow #/ A0
    # outflow <- outflow # / A0
    # dates <- seq.Date(from = dates[1], by = 1, length.out = length(dates))
    out_list[["LKE_evpflx"]] <- EVAP / 86400
    out_list[["LKE_evpvol"]] <- EVAP * out_list[["LKE_A0"]]
    out_list[["LKE_pcpvol"]] <- precip * out_list[["LKE_A0"]]
    
    # Light
    # efold <- rep(NA, length(idx))
    # euphotic <- rep(NA, length(idx))
  }
  
  out_list <- c(out_list, list(ok = TRUE, reason = NULL))
  return(out_list)
}

#' Read DYRESM water level output
#' 
#' @inheritParams read_dy_output
#' @return Data frame with Date and LKE_lvlwtr columns
#' @export
#' @importFrom ncdf4 ncvar_get ncatt_get nc_close
#' @importFrom withr local_locale local_timezone
#' @importFrom cli cli_abort
read_dy_wlev <- function(nc = NULL, file) {
  # Set timezone
  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")
  
  if (is.null(nc)) {
    nc <- open_nc_safe(file, model = "dy_cd")
    on.exit(ncdf4::nc_close(nc))
  }
  
  if (!("dyresmTime" %in% names(nc$var))) {
    cli::cli_abort("No time dimension found in DYRESM output")
  }
  dy_time <- ncdf4::ncvar_get(nc, "dyresmTime")
  dy_time[dy_time > 9.9e36] <- NA
  dy_dates <- as.POSIXct((dy_time - 2415018.5) *
                           86400, origin = "1899-12-30") |> as.Date()
  
  mod_layers <- ncdf4::ncvar_get(nc, "dyresmLAYER_HTS_Var")
  if (!is.matrix(mod_layers)) {
    cli_inform_safe("Error reading DYRESM layers, potentially due to water level
                   fluctuations.\nReturning NULL...")
    return(NULL)
  }
  
  lake_level <- apply(mod_layers, 2, \(x) max(x, na.rm = TRUE))
  
  out_df <- data.frame(Date = dy_dates,
                       LKE_lvlwtr = lake_level)
  return(out_df)
}
