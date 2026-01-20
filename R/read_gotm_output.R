#' Read GOTM output
#'
#' @inheritParams read_glm_output
#'
#' @returns List with AEME output variables
#' @export
#'
#' @importFrom ncdf4 ncvar_get ncatt_get nc_close
#' @importFrom lubridate hour

read_gotm_output <- function(nc = NULL, vars_sim = NULL, depths = NULL, 
                             dates = NULL, date_index = NULL,
                             incl_fluxes = FALSE, output_hour = 0, file) {
  if (is.null(nc)) {
    nc <- open_nc_safe(file, model = "gotm_wet")
    on.exit(ncdf4::nc_close(nc))
  }
  out_list <- list()
  ## dates.. gotm seems to output the intial profiles, then every tstep
  out_steps <- ncdf4::ncvar_get(nc, "time")
  if (length(out_steps) == 0) {
    out <- empty_model_output(reason = "Empty time dimension")
    return(out)
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
  if (length(gotm_dates) < max(date_index)) {
    cli::cli_alert_warning("date_index exceeds available GOTM output dates. 
                          Returning empty output.")
    out <- empty_model_output(
      reason = "date_index exceeds available GOTM output dates"
    )
    return(out)
  }
  
  t_start <- date_index[1]
  start_1d <- c(1, 1, t_start)
  start_2d <- c(1, 1, 1, t_start)
  
  dates <- gotm_dates[date_index] |> as.Date()
  
  # dates <- seq.Date(date_start, by = 1, length.out = length(out_steps))
  lyr_h <- ncdf4::ncvar_get(nc, "h")[, date_index] # lyrs
  zeta <- ncdf4::ncvar_get(nc, "zeta")[date_index]
  zi <- ncdf4::ncvar_get(nc, "zi")[, date_index]
  z <- ncdf4::ncvar_get(nc, "z")[, date_index]
  
  sst <- ncdf4::ncvar_get(nc, "sst")[date_index]
  if (sum(is.na(sst)) > 0) { # sum(sst == 0) > 1
    # Run-length encoding of the vector
    sst[is.na(sst)] <- -999
    rle_result <- rle(as.vector(sst))
    start_index <- which(rle_result$lengths > 1)[1]
    if (is.na(start_index)) {
      warning(strwrap(paste0("There are ", sum(sst == 0),
                             " SST values of 0 in the GOTM output. Not
                               removing any output.")))
    } else {
      # vals <- rle_result$values[start_index]
      z[, start_index:ncol(z)] <- NA
      zi[, start_index:ncol(zi)] <- NA
      lyr_h[, start_index:ncol(lyr_h)] <- NA
    }
  }
  lake_level <- zi[nrow(zi), ] - zi[1, ]
  lake_level[lake_level <= 0] <- 0
  zeta[lake_level <= 0] <- NA
  # lake_level <- data.frame(ncdf4::ncvar_get(nc, "z"))
  lyrs <- z # lyrs
  lyrs[1, ] <- lyrs[1, ] - (lyr_h[1, ] / 2)
  lyrs[nrow(lyrs), ] <- lyrs[nrow(lyrs), ] + (lyr_h[nrow(lyr_h), ] / 2)
  lyrs <- apply(lyrs, 2, \(x) x + abs(min(x)))
  
  if (is.null(depths)) {
    max_depth <- max(lake_level, na.rm = TRUE)
    data("model_layer_structure", package = "AEME", envir = environment())
    depth_fraction <- model_layer_structure |> 
      dplyr::filter(z < max_depth) |> 
      dplyr::mutate(deps = z / max_depth) |> 
      dplyr::pull(deps) |> 
      matrix(ncol = 1)
    depth_mat <- depth_fraction %*% t(lake_level)
    out_depths <- round(depth_mat, 2)
  } else {
    out_depths <- matrix(rep(depths, length(dates)),
                         nrow = length(depths),
                         ncol = length(dates))
  }
  
  Lmat <- matrix(zeta, nrow = nrow(z), ncol = length(zeta),
                 byrow = TRUE)
  Lmat_zi <- matrix(zeta, nrow = nrow(zi), ncol = length(zeta),
                    byrow = TRUE)
  
  midpoints <- Lmat - z
  midpoints_zi <- Lmat_zi - zi
  
  # norm_depths <- depths / max(depths)
  # depth_mat <- matrix(data = norm_depths, ncol = 1)
  # level_mat <- matrix(data = depth, nrow = 1, ncol = length(date_index))
  # 
  # # Multiply matrices to get actual depths
  # out_depths <- depth_mat %*% level_mat
  
  out_list[["Date"]] <- dates
  out_list[["LKE_lvlwtr"]] <- as.vector(lake_level)
  out_list[["LKE_depths"]] <- as.matrix(out_depths)
  if ("LKE_photic" %in% vars_sim | "LKE_efold" %in% vars_sim) {
    # Light
    rad <- ncdf4::ncvar_get(nc, "rad")[, date_index]
    efold <- sapply(seq_len(ncol(rad)), \(t) {
      if (t == 1 | all(rad[, t] == 0)) return(0) # Day 1 is always 0
      if (sum(complete.cases(rad[, t], zi[, t])) < 2) return(NA)
      if (length(unique(rad[complete.cases(rad[, t], zi[, t]), t])) < 2) return(NA)
      zeta[t] - approx(rad[, t], zi[, t], xout = (1/exp(1) * rad[nrow(rad), t]))$y
    })
    euphotic <- sapply(seq_len(ncol(rad)), \(t) {
      if (t == 1 | all(rad[, t] == 0)) return(0) # Day 1 is always 0
      if (sum(complete.cases(rad[, t], zi[, t])) < 2) return(NA)
      if (length(unique(rad[complete.cases(rad[, t], zi[, t]), t])) < 2) return(NA)
      zeta[t] - approx(rad[, t], zi[, t], xout = (0.01 * rad[nrow(rad), t]))$y
    })
    
    out_list[["LKE_efold"]] <- as.vector(abs(efold))
    out_list[["LKE_photic"]] <- as.vector(abs(euphotic))
    vars_sim <- vars_sim[!vars_sim %in% c("LKE_photic", "LKE_efold")]
  }
  
  
  if (!is.null(vars_sim)) {
    model_vars <- get_model_vars(vars_sim = vars_sim, model = "gotm_wet")
    model_vars_vec <- format_model_vars_vec(vars_sim = vars_sim, 
                                            model = "gotm_wet")
    nc_vars <- names(nc$var)
    vars_chk <- data.frame(vars = model_vars_vec,
                           present = model_vars_vec %in% nc_vars)
    
    out_vars <- lapply(model_vars_vec, \(v) {
      if(vars_chk$present[vars_chk$vars == v] == FALSE) {
        return(NULL)
      }
      var <- ncdf4::ncvar_get(nc, v)[, date_index]
      if (nrow(var) == nrow(zi)) {
        interp_static_grid(var = var,
                           midpoints = midpoints_zi,
                           out_depths = out_depths)
      } else if (nrow(var) == nrow(z)) {
        interp_static_grid(var = var,
                           midpoints = midpoints,
                           out_depths = out_depths)
      }
    })
    
    out_list <- c(out_list, out_vars)
  }
  
  if (incl_fluxes) {
    flux_list <- read_gotm_flux_output(nc = nc, date_index = date_index)
    # Add missing vars to out_list
    missing_vars <- setdiff(names(flux_list), names(out_list))
    flux_list <- flux_list[missing_vars]
    out_list <- c(out_list, flux_list)
  }
  
  out_list <- c(out_list, list(ok = TRUE, reason = NULL))
  return(out_list)
}

#' Read GOTM water level output
#' 
#' @inheritParams read_gotm_output
#' @returns Data frame with Date and LKE_lvlwtr columns
#' @export
#' @importFrom ncdf4 ncvar_get ncatt_get nc_close
read_gotm_wlev <- function(nc = NULL, file) {
  if (is.null(nc)) {
    nc <- open_nc_safe(file, model = "gotm_wet")
    on.exit(ncdf4::nc_close(nc))
  }
  out_steps <- ncdf4::ncvar_get(nc, "time")
  if (length(out_steps) == 0) {
    cli::cli_abort("No time dimension in GOTM output")
  }
  date_start <- ncdf4::ncatt_get(nc, "time", "units")$value |>
    gsub("seconds since ", "", x = _) |>
    as.POSIXct()
  time_vec <- ncdf4::ncvar_get(nc, "time")
  gotm_dates <- as.POSIXct(time_vec + date_start) |> 
    as.Date()
  
  zi <- ncdf4::ncvar_get(nc, "zi")
  zeta <- ncdf4::ncvar_get(nc, "zeta")
  
  lake_level <- zi[nrow(zi), ] - zi[1, ]
  lake_level[lake_level <= 0] <- 0
  zeta[lake_level <= 0] <- NA
  
  out_df <- data.frame(Date = gotm_dates,
                       LKE_lvlwtr = as.vector(lake_level))
  return(out_df)
} 
