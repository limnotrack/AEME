#' Read GLM netCDF output
#'
#' @inheritParams ncdf4::ncvar_get
#' @param vars_sim Variables to extract in the AEME format e.g. "HYD_temp"
#' @param depths Depths to extract. If NULL, extract all model layer depths. 
#' Defaults to NULL.
#' @param dates Dates to extract. If NULL, extract all dates. Defaults to NULL.
#' @param date_index Date index to extract. If NULL, extract all dates. Defaults
#' to NULL.
#' @param incl_fluxes Logical indicating whether to include flux variables.
#' Defaults to TRUE.
#' @param output_hour Hour of the day to extract (0-23). Defaults to 0.
#' @param file File path to netCDF file. Only used if `nc` is NULL.
#'
#' @returns List with AEME output variables
#' @export
#' 
#' @importFrom ncdf4 ncvar_get ncatt_get
#' @importFrom lubridate hour
#' @importFrom dplyr filter mutate pull

read_glm_output <- function(nc = NULL, vars_sim = NULL, depths = NULL,
                            dates = NULL, date_index = NULL, incl_fluxes = TRUE, 
                            output_hour = 0, file) {
  
  if (is.null(nc)) {
    nc <- open_nc_safe(file, model = "glm_aed")
    on.exit(ncdf4::nc_close(nc))
  }
  out_list <- list()
  # glm DOES NOT output initial profiles
  hours_since  <- ncdf4::ncvar_get(nc, "time")
  if (length(hours_since) == 0) {
    return(NULL)
  }
  date_start <- as.POSIXct(gsub("hours since ", "",
                                ncdf4::ncatt_get(nc,'time','units')$value))
  glm_dates <- as.POSIXct(hours_since * 3600 + date_start) |> 
    as.Date()
  if (is.null(date_index)) {
    if (!is.null(dates)) {
      date_index <- which(glm_dates %in% dates)
      if (length(date_index) == 0) {
        cli::cli_abort("No output for GLM at specified dates")
      }
    } else {
      date_index <- seq_along(glm_dates)
    }
  }
  dates <- glm_dates[date_index] |>
    as.Date()

  # Extract depths and format
  mod_layers <- ncdf4::ncvar_get(nc, "z")[, date_index]
  mod_layers[mod_layers > 1000000] <- NA
  midpoints <- apply(mod_layers, 2, \(x) {
    x - diff(c(0, x)) / 2
  })
  lake_level <- ncdf4::ncvar_get(nc, "lake_level")[date_index]
  # Adjust midpoints to be relative to lake level
  Lmat <- matrix(lake_level, nrow = nrow(midpoints), ncol = length(lake_level),
                 byrow = TRUE)
  midpoints <- Lmat - midpoints
  out_list[["LKE_lvlwtr"]] <- lake_level
  
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
  
  # Extract flux variables
  if (incl_fluxes) {
    out_list[["LKE_Qe"]] <- ncdf4::ncvar_get(nc, "daily_qe")[date_index]
    out_list[["LKE_Qh"]] <- ncdf4::ncvar_get(nc, "daily_qh")[date_index]
    out_list[["LKE_Qlw"]] <- ncdf4::ncvar_get(nc, "daily_qlw")[date_index]
    out_list[["LKE_Qsw"]] <- ncdf4::ncvar_get(nc, "daily_qsw")[date_index]
    out_list[["LKE_V"]] <- ncdf4::ncvar_get(nc, "lake_volume")[date_index]
    out_list[["LKE_evpvol"]] <- -ncdf4::ncvar_get(nc, "evaporation")[date_index]
    out_list[["LKE_evpflx"]] <- -ncdf4::ncvar_get(nc, "evap_mass_flux")[date_index]
    out_list[["LKE_A0"]] <- ncdf4::ncvar_get(nc, "surface_area")[date_index]
    out_list[["LKE_evprte"]] <- abs(out_list[["LKE_evpvol"]] / 
                                      out_list[["LKE_A0"]])
    out_list[["LKE_inflow"]] <- ncdf4::ncvar_get(nc, "tot_inflow_vol")[date_index] # / A0
    out_list[["LKE_outflow"]] <- (ncdf4::ncvar_get(nc, "tot_outflow_vol")[date_index] +
                                    ncdf4::ncvar_get(nc, "overflow_vol")[date_index]) # / A0
    out_list[["LKE_precip"]] <- ncdf4::ncvar_get(nc, "precipitation")[date_index]
    out_list[["LKE_pcpvol"]] <- out_list[["LKE_precip"]] * out_list[["LKE_A0"]]
    out_list[["HYD_surft"]] <- ncdf4::ncvar_get(nc, "surface_temp")[date_index]
    
    rad <- ncdf4::ncvar_get(nc, "radn")[, date_index]
    rad <- interp_static_grid(var = rad,
                              midpoints = midpoints,
                              out_depths = out_depths)
    suppressWarnings({
      out_list[["LKE_efold"]] <- sapply(seq_len(ncol(rad)), \(t) {
        if (sum(!is.na(rad[, t])) < 2 | length(unique(out_depths[, t])) <= 1 |
            length(unique(rad[, t])) <= 1) {
          return(NA)
        }
        ref_rad <- (1/exp(1) * max(rad[, t]))
        approx(rad[, t], out_depths[, t], xout = ref_rad)$y
      })
      out_list[["LKE_photic"]] <- sapply(seq_len(ncol(rad)), \(t) {
        if (sum(!is.na(rad[, t])) < 2 | length(unique(out_depths[, t])) <= 1 |
            length(unique(rad[, t])) <= 1) {
          return(NA)
        }
        ref_rad <- (0.01 * max(rad[, t]))
        approx(rad[, t], out_depths[, t], xout = ref_rad)$y
      })
    })
    
  }
  out_list <- lapply(out_list, as.vector)
  out_list[["Date"]] <- dates
  
  
  # Add depths as a matrix
  out_list[["LKE_depths"]] <- out_depths
  
  
  if (!is.null(vars_sim)) {
    model_vars <- get_model_vars(vars_sim = vars_sim, model = "glm_aed")
    model_vars_vec <- format_model_vars_vec(vars_sim = vars_sim, 
                                            model = "glm_aed")
    nc_vars <- names(nc$var)
    vars_chk <- data.frame(vars = model_vars_vec,
                           present = model_vars_vec %in% nc_vars,
                           conv_factor = model_vars$conversion_aed
    )
    
    out_vars <- lapply(model_vars_vec, \(v) {
      if(vars_chk$present[vars_chk$vars == v] == FALSE) {
        return(NULL)
      }
      conv_factor <- vars_chk$conv_factor[vars_chk$vars == v]
      if (is.na(conv_factor)) {
        conv_factor <- 1
      }
      var <- ncdf4::ncvar_get(nc, v)[, date_index] * conv_factor
      interp_static_grid(var = var,
                         midpoints = midpoints,
                         out_depths = out_depths)
    })
    
    out_list <- c(out_list, out_vars)
  }
  return(out_list)
}
