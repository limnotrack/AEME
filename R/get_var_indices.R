#' Get variable indices
#'
#' Get variable indices for an AEME variable.
#'
#' @inheritParams ncdf4::nc_close
#' @inheritParams build_aeme
#' @param vars_sim character; vector of AEME variable names to get indices for.
#' @param month numeric; vector of months to subset the data.
#' @param depth_range numeric; vector of depth ranges, length two to subset the
#'  data.
#'
#' @return list; of variable indices.
#' @export
#'

get_var_indices <- function(nc = NULL, model, aeme, path, vars_sim,
                            month = NULL, depth_range = NULL) {

  # Check function args ----
  if (length(model) != 1) {
    stop("model must be a single string.")
  }

  # Get AEME time variable ----
  aeme_time <- AEME::time(aeme)

  # If nc is not provided access it using aeme and model ----
  if (is.null(nc)) {
    out_file <- AEME::get_model_outfile(aeme = aeme, model = model,
                                        path = path)[[model]]
    if (length(out_file) == 2) {
      out_file <- out_file[1]
    }
    if (!file.exists(out_file)) {
      stop("No ", out_file, " present.")
    }
    nc <- ncdf4::nc_open(out_file, return_on_error = TRUE)
    on.exit(ncdf4::nc_close(nc))
  }

  # Get model time ----
  if (model == "dy_cd") {
    dates <- as.POSIXct((ncdf4::ncvar_get(nc, 'dyresmTime') - 2415018.5) *
                          86400,
                        origin = "1899-12-30", tz = "UTC") |>
      as.Date()
  } else if (model == "glm_aed") {
    hours_since  <- ncdf4::ncvar_get(nc, "time")
    date_start <- as.POSIXct(gsub("hours since ", "",
                                  ncdf4::ncatt_get(nc, "time", "units")$value),
                             tz = "UTC")
    dates <- as.Date(hours_since * 3600 + date_start)
  } else if (model == 'gotm_pclake' | model == "gotm_wet") {
    out.steps <- ncdf4::ncvar_get(nc, "time")
    date_start <- ncdf4::ncatt_get(nc,'time','units')$value |>
      gsub("seconds since ", "", x = _) |>
      as.POSIXct() |>
      as.Date()
    dates <- seq.Date(date_start, by = 1, length.out = length(out.steps))
  }

  # Trim off spinup time
  # dates <- dates[dates >= aeme_time$start & dates <= aeme_time$stop]


  # If month and depth_range are not provided, use aeme observation month and depth_range
  if (is.null(month) & is.null(depth_range)) {
    obs <- AEME::observations(aeme)
    var_indices <- lapply(vars_sim, \(v) {
      obs_v <- obs$lake |>
        dplyr::filter(var_aeme == v & Date %in% dates) |>
        dplyr::mutate(depth_mid = (depth_from + depth_to) / 2)
      deps <- unique(obs_v$depth_mid)
      deps <- deps[order(deps)]
      date_idx <- which(dates %in% obs_v$Date)
      list(time = date_idx, depths = deps, dates = dates[date_idx])
    })
  } else {
    var_indices <- lapply(vars_sim, \(v) {
      deps <- seq(min(depth_range),
                  max(depth_range), by = 0.5)
      df <- data.frame(dates = dates, month = lubridate::month(dates))

      date_idx <- which(df$month %in% month)
      list(time = date_idx, depths = deps, dates = dates[date_idx])
    })
  }
  names(var_indices) <- vars_sim
  return(var_indices)
}
