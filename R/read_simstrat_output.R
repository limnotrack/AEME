#' Read Simstrat-AED2 netCDF output
#'
#' Reads the consolidated `output.nc` produced by
#' \code{\link{write_simstrat_nc}} (Simstrat itself writes one text `.dat`
#' file per variable; AEME converts these to netCDF as a post-processing
#' step so the same reading approach as GLM-AED/GOTM-WET can be reused).
#'
#' @inheritParams read_glm_output
#'
#' @returns List with AEME output variables
#' @export
#'
#' @importFrom ncdf4 ncvar_get ncatt_get
#' @importFrom dplyr filter mutate pull left_join rename
read_simstrat_output <- function(nc = NULL, vars_sim = NULL, depths = NULL,
                                 dates = NULL, date_index = NULL,
                                 incl_fluxes = TRUE, output_hour = 0, file,
                                 phyto_pars = NULL) {

  if (is.null(nc)) {
    nc <- open_nc_safe(file, model = "simstrat_aed2")
    on.exit(ncdf4::nc_close(nc))
  }
  out_list <- list()

  time_sec <- ncdf4::ncvar_get(nc, "time")
  if (length(time_sec) == 0) {
    out <- empty_model_output(reason = "Empty time dimension")
    return(out)
  }
  date_start <- as.POSIXct(gsub("seconds since ", "",
                                ncdf4::ncatt_get(nc, "time", "units")$value),
                           tz = "UTC")
  simstrat_dates <- as.POSIXct(time_sec, origin = date_start, tz = "UTC") |>
    as.Date()

  if (is.null(date_index)) {
    if (!is.null(dates)) {
      date_index <- which(simstrat_dates %in% dates)
      if (length(date_index) == 0) {
        cli::cli_abort("No output for Simstrat-AED2 at specified dates")
      }
    } else {
      date_index <- seq_along(simstrat_dates)
    }
  }
  if (length(simstrat_dates) < max(date_index)) {
    cli::cli_alert_warning("date_index exceeds available Simstrat-AED2 output
                          dates. Returning empty output.")
    out <- empty_model_output(
      reason = "date_index exceeds available Simstrat-AED2 output dates"
    )
    return(out)
  }
  dates <- simstrat_dates[date_index] |> as.Date()

  nc_vars <- names(nc$var)

  # Simstrat's output grid is static (unlike GLM's time-varying layers), so
  # the same depth vector applies at every timestep
  z <- ncdf4::ncvar_get(nc, "z")
  midpoints <- matrix(rep(z, length(date_index)), ncol = length(date_index))

  lake_level <- if ("WaterH" %in% nc_vars) {
    ncdf4::ncvar_get(nc, "WaterH")[date_index]
  } else {
    rep(max(z, na.rm = TRUE), length(date_index))
  }
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

  if (incl_fluxes) {
    if ("HK" %in% nc_vars) out_list[["LKE_Qh"]]  <- ncdf4::ncvar_get(nc, "HK")[date_index]
    if ("HV" %in% nc_vars) out_list[["LKE_Qe"]]  <- ncdf4::ncvar_get(nc, "HV")[date_index]
    if ("Rad0" %in% nc_vars) out_list[["LKE_Qsw"]] <- ncdf4::ncvar_get(nc, "Rad0")[date_index]
    if ("T" %in% nc_vars) {
      out_list[["HYD_surft"]] <- ncdf4::ncvar_get(nc, "T")[1, date_index]
    }
  }

  out_list <- lapply(out_list, as.vector)
  out_list[["Date"]] <- dates
  out_list[["LKE_depths"]] <- out_depths

  if (!is.null(vars_sim)) {
    model_vars <- get_model_vars(vars_sim = vars_sim, model = "simstrat_aed2")
    model_vars_vec <- get_model_vars(vars_sim = vars_sim, model = "simstrat_aed2",
                                     as_vector = TRUE)

    vars_chk <- data.frame(
      vars = model_vars_vec,
      present = model_vars_vec %in% nc_vars
    ) |>
      dplyr::left_join(model_vars, by = c("vars" = "simstrat_aed2")) |>
      dplyr::rename(conv_factor = conversion_aed)

    out_vars <- lapply(model_vars_vec, \(v) {
      if (isFALSE(vars_chk$present[vars_chk$vars == v])) {
        return(NULL)
      }
      conv_factor <- vars_chk$conv_factor[vars_chk$vars == v]
      if (is.na(conv_factor)) conv_factor <- 1

      var_out <- ncdf4::ncvar_get(nc, v)
      if (is.null(dim(var_out)) || length(dim(var_out)) == 1) {
        # Surface-only (time-only) variable
        return(var_out[date_index] * conv_factor)
      } else if (length(dim(var_out)) == 2) {
        var <- var_out[, date_index, drop = FALSE] * conv_factor
        out <- interp_static_grid(var = var, midpoints = midpoints,
                                  out_depths = out_depths)
        return(out)
      } else {
        cli::cli_abort(paste("Variable", v, "has unsupported number of dimensions"))
      }
    })

    out_list <- c(out_list, out_vars)
  }
  out_list <- c(out_list, list(ok = TRUE, reason = NULL))
  return(out_list)
}

#' Read Simstrat-AED2 lake water level output
#'
#' @inheritParams read_simstrat_output
#' @returns Data frame with Date and LKE_lvlwtr columns
#' @export
#' @importFrom ncdf4 ncvar_get ncatt_get
read_simstrat_wlev <- function(nc = NULL, file) {
  if (is.null(nc)) {
    nc <- open_nc_safe(file, model = "simstrat_aed2")
    on.exit(ncdf4::nc_close(nc))
  }
  time_sec <- ncdf4::ncvar_get(nc, "time")
  if (length(time_sec) == 0) {
    cli::cli_abort("No time dimension in Simstrat-AED2 output")
  }
  date_start <- as.POSIXct(gsub("seconds since ", "",
                                ncdf4::ncatt_get(nc, "time", "units")$value),
                           tz = "UTC")
  dates <- as.POSIXct(time_sec, origin = date_start, tz = "UTC") |> as.Date()

  lake_level <- if ("WaterH" %in% names(nc$var)) {
    ncdf4::ncvar_get(nc, "WaterH")
  } else {
    rep(NA_real_, length(dates))
  }

  data.frame(Date = dates, LKE_lvlwtr = lake_level)
}
