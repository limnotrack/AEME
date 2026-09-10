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
#' @param use_obs logical; if TRUE, use the observation months and depth ranges
#' from the AEME object.
#'
#' @return list; of variable indices. Each list element corresponds to a 
#' variable in vars_sim and contains a list with time indices, depth values, and 
#' dates. Time indices correspond to the positions in the model output time 
#' series that match the Date but are the corresponding index in the model 
#' output. 
#' @export

get_var_indices <- function(nc = NULL, model, aeme, path, vars_sim,
                            month = NULL, depth_range = NULL, use_obs = TRUE) {

  # Model output time axes are UTC (CF "<unit> since <origin>"); keep all
  # datetime arithmetic here in UTC too.
  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")

  # Check function args ----
  aeme <- check_aeme(aeme)
  model <- check_model(model = model)
  path <- check_path(path = path, must_exist = TRUE)
  if (length(model) != 1) {
    stop("model must be a single string.")
  }

  # Get AEME time variable ----
  aeme_time <- AEME::time(aeme)

  # If nc is not provided access it using aeme and model ----
  if (is.null(nc)) {
    out_file <- get_model_outfile(aeme = aeme, model = model)[[model]]
    if (length(out_file) == 2) {
      out_file <- out_file[1]
    }
    if (!file.exists(out_file)) {
      stop("No ", out_file, " present.")
    }
    nc <- ncdf4::nc_open(out_file, return_on_error = TRUE)
    on.exit(ncdf4::nc_close(nc))
  }

  # Get model time (POSIXct; collapsed to Date when the output is daily) ----
  if (model == "dy_cd") {
    dates <- as.POSIXct((ncdf4::ncvar_get(nc, 'dyresmTime') - 2415018.5) *
                          86400,
                        origin = "1899-12-30", tz = "UTC")
  } else if (model == "glm_aed") {
    hours_since  <- ncdf4::ncvar_get(nc, "time")
    date_start <- as.POSIXct(gsub("hours since ", "",
                                  ncdf4::ncatt_get(nc, "time", "units")$value),
                             tz = "UTC")
    dates <- as.POSIXct(hours_since * 3600 + date_start, tz = "UTC")
  } else if (model == 'gotm_pclake' | model == "gotm_wet") {
    out.steps <- ncdf4::ncvar_get(nc, "time")
    date_start <- ncdf4::ncatt_get(nc,'time','units')$value |>
      gsub("seconds since ", "", x = _) |>
      as.POSIXct(tz = "UTC")
    dates <- as.POSIXct(out.steps + date_start, tz = "UTC")
  } else if (model %in% c("simstrat_aed2", "simstrat_aed")) {
    seconds_since <- ncdf4::ncvar_get(nc, "time")
    date_start <- as.POSIXct(gsub("seconds since ", "",
                                  ncdf4::ncatt_get(nc, "time", "units")$value),
                             tz = "UTC")
    dates <- as.POSIXct(seconds_since, origin = date_start, tz = "UTC")
  }
  dates <- .collapse_output_date(dates)

  # Trim off spinup time
  # dates <- dates[dates >= aeme_time$start & dates <= aeme_time$stop]


  # If month and depth_range are not provided, use aeme observation month and depth_range
  if (is.null(month) & is.null(depth_range) & use_obs) {
    obs <- AEME::observations(aeme)
    # Observations are daily (stored as noon POSIXct); match the model axis on
    # the calendar day. For a sub-daily axis keep one step per obs day.
    dates_day <- as.Date(dates, tz = "UTC")
    var_indices <- lapply(vars_sim, \(v) {
      obs_v <- obs$lake |>
        dplyr::mutate(Date = as.Date(Date, tz = "UTC")) |>
        dplyr::filter(var_aeme == v & Date %in% dates_day) |>
        dplyr::mutate(depth_mid = depth)
      deps <- unique(obs_v$depth_mid)
      deps <- deps[order(deps)]
      date_idx <- which(dates_day %in% obs_v$Date & !duplicated(dates_day))
      list(date_index = date_idx, depths = deps, dates = dates[date_idx])
    })
  } else if (is.null(month) & is.null(depth_range)) {
    lke <- lake(aeme)
    deps <- get_model_layers(depth = lke$depth) |> 
      dplyr::pull(z)
    deps <- c(0, deps)
    var_indices <- lapply(vars_sim, \(v) {
      date_idx <- which(dates %in% obs_v$Date)
      list(date_index = date_idx, depths = deps, dates = dates[date_idx])
    })
  } else {
    var_indices <- lapply(vars_sim, \(v) {
      deps <- seq(min(depth_range),
                  max(depth_range), by = 0.5)
      df <- data.frame(dates = dates, month = lubridate::month(dates))

      date_idx <- which(df$month %in% month)
      list(date_index = date_idx, depths = deps, dates = dates[date_idx])
    })
  }
  names(var_indices) <- vars_sim
  return(var_indices)
}
