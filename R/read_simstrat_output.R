#' Read Simstrat-AED2 netCDF output
#'
#' Reads the consolidated `output.nc` produced by
#' \code{\link{write_simstrat_nc}} (Simstrat itself writes one text `.dat`
#' file per variable; AEME converts these to netCDF as a post-processing
#' step so the same reading approach as GLM-AED/GOTM-WET can be reused).
#'
#' @inheritParams read_glm_output
#' @param model character; which Simstrat coupling this output came from,
#' `"simstrat_aed2"` (default) or `"simstrat_aed"`. Selects the matching
#' `key_naming` column for variable-name translation -- the netCDF file
#' format itself (produced by \code{\link{write_simstrat_nc}}) is identical
#' either way.
#'
#' @returns List with AEME output variables
#' @export
#'
#' @importFrom ncdf4 ncvar_get ncatt_get
#' @importFrom dplyr filter mutate pull left_join rename
read_simstrat_output <- function(nc = NULL, vars_sim = NULL, depths = NULL,
                                 dates = NULL, date_index = NULL,
                                 incl_fluxes = TRUE, output_hour = 0, file,
                                 phyto_pars = NULL, load_all = TRUE,
                                 raw_output = FALSE, model = "simstrat_aed2") {

  if (isTRUE(raw_output) && !is.null(depths)) {
    cli::cli_abort("'depths' cannot be supplied when 'raw_output = TRUE' -- raw output uses Simstrat's native output depths.")
  }

  if (is.null(nc)) {
    nc <- open_nc_safe(file, model = model)
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
    if (isTRUE(raw_output)) {
      # raw mode: report Simstrat-AED2's own native output depths, rather
      # than interpolating onto a shared standardised grid
      out_depths <- round(midpoints, 2)
    } else {
      max_depth <- max(lake_level, na.rm = TRUE)
      data("model_layer_structure", package = "AEME", envir = environment())
      depth_fraction <- model_layer_structure |>
        dplyr::filter(z < max_depth) |>
        dplyr::mutate(deps = z / max_depth) |>
        dplyr::pull(deps) |>
        matrix(ncol = 1)
      depth_mat <- depth_fraction %*% t(lake_level)
      out_depths <- round(depth_mat, 2)
    }
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
    model_vars <- get_model_vars(vars_sim = vars_sim, model = model)
    model_vars_vec <- get_model_vars(vars_sim = vars_sim, model = model,
                                     as_vector = TRUE)

    vars_chk <- data.frame(
      vars = model_vars_vec,
      present = model_vars_vec %in% nc_vars
    ) |>
      dplyr::left_join(model_vars, by = stats::setNames(model, "vars")) |>
      dplyr::rename(conv_factor = conversion_aed)

    out_vars <- lapply(model_vars_vec, \(v) {
      if (isFALSE(vars_chk$present[vars_chk$vars == v])) {
        return(NULL)
      }
      # AED unit-conversion factors are an AEME-specific transform, only
      # applied when standardising output -- raw output stays in Simstrat/
      # AED's own units, matching the netCDF file exactly
      if (isTRUE(raw_output)) {
        conv_factor <- 1
      } else {
        conv_factor <- vars_chk$conv_factor[vars_chk$vars == v]
        if (is.na(conv_factor)) conv_factor <- 1
      }

      var_out <- ncdf4::ncvar_get(nc, v)
      if (is.null(dim(var_out)) || length(dim(var_out)) == 1) {
        # Surface-only (time-only) variable
        return(var_out[date_index] * conv_factor)
      } else if (length(dim(var_out)) == 2) {
        var <- var_out[, date_index, drop = FALSE] * conv_factor
        out <- .glm_depth_profile(var = var, midpoints = midpoints,
                                  out_depths = out_depths,
                                  raw_output = raw_output)
        return(out)
      } else {
        cli::cli_abort(paste("Variable", v, "has unsupported number of dimensions"))
      }
    })

    if (isTRUE(raw_output)) {
      # raw mode: key by the native Simstrat-AED2/netCDF variable name (e.g.
      # "T") instead of the translated AEME var_aeme name (e.g. "HYD_temp")
      names(out_vars) <- unname(model_vars_vec)
    }

    out_list <- c(out_list, out_vars)
  }

  # ---- Load every remaining variable present in the file ----
  # Variables already handled above (by the fixed-name blocks and, if
  # vars_sim was supplied, the declared/translated loop) are skipped;
  # everything else in the file is loaded too -- keyed by its var_aeme
  # name if key_naming has a translation, otherwise by its raw Simstrat-AED2
  # name.
  if (isTRUE(load_all)) {
    already_extracted <- c("time", "z", "WaterH")
    if (incl_fluxes) {
      already_extracted <- c(already_extracted, "HK", "HV", "Rad0")
    }
    if (!is.null(vars_sim)) {
      already_extracted <- c(already_extracted, model_vars_vec)
    }

    data("key_naming", package = "AEME", envir = environment())
    sim_to_var_aeme <- stats::setNames(key_naming$var_aeme, key_naming[[model]])

    remaining_vars <- setdiff(nc_vars, already_extracted)

    for (v in remaining_vars) {
      key <- unname(sim_to_var_aeme[v])
      if (is.na(key) || !nzchar(key)) key <- v
      if (key %in% names(out_list)) next

      # AED unit-conversion factors are an AEME-specific transform, only
      # applied when standardising output -- raw output stays in Simstrat/
      # AED's own units, matching the netCDF file exactly
      if (isTRUE(raw_output)) {
        conv_factor <- 1
      } else {
        conv_idx <- match(v, key_naming[[model]])
        conv_factor <- if (!is.na(conv_idx)) key_naming$conversion_aed[conv_idx] else NA
        if (is.na(conv_factor)) conv_factor <- 1
      }

      result <- tryCatch({
        dim_objs  <- Filter(\(d) d$len > 1, nc$var[[v]]$dim)
        dim_names <- vapply(dim_objs, \(d) d$name, character(1))

        if (setequal(dim_names, "time")) {
          var_out <- ncdf4::ncvar_get(nc, v)
          as.vector(var_out[date_index] * conv_factor)
        } else if (setequal(dim_names, c("z", "time"))) {
          var_out <- ncdf4::ncvar_get(nc, v)
          if (dim_names[1] != "z") var_out <- t(var_out)
          var <- var_out[, date_index, drop = FALSE] * conv_factor
          .glm_depth_profile(var = var, midpoints = midpoints,
                             out_depths = out_depths,
                             raw_output = raw_output)
        } else {
          .read_glm_grouped_var(nc = nc, v = v, dim_objs = dim_objs,
                                dim_names = dim_names, date_index = date_index,
                                dates = dates)
        }
      }, error = function(e) {
        cli::cli_warn(c("!" = "Could not read variable {.val {v}} from Simstrat output: {conditionMessage(e)}"))
        NULL
      })

      if (!is.null(result)) {
        out_list[[key]] <- result
      }
    }
  }

  if (isTRUE(raw_output)) {
    # "Date"/"LKE_depths"/"ok"/"reason" are the output list's own structural
    # keys, not plotted variables -- key_naming does have a real "Date" ->
    # "time" translation (used elsewhere for a genuinely different purpose),
    # so leaving them in this sweep would rename "Date" itself out from
    # under every consumer that expects a stable key
    out_names <- setdiff(names(out_list), c("Date", "LKE_depths", "ok", "reason"))
    var_names <- get_model_vars(out_names, model = model, as_vector = TRUE)
    for (i in seq_along(var_names)) {
      if (!is.na(var_names[i]) && nzchar(var_names[i])) {
        names(out_list)[names(out_list) == names(var_names)[i]] <- var_names[i]
      }
    }
  }

  out_list <- c(out_list, list(ok = TRUE, reason = NULL))

  var_units <- var_long_name <- NULL
  if (isTRUE(raw_output)) {
    raw_vars <- setdiff(names(out_list), c("Date", "LKE_depths", "ok", "reason"))
    meta <- lapply(raw_vars, \(v) .nc_var_meta(nc, v))
    var_units <- stats::setNames(vapply(meta, `[[`, "", "units"), raw_vars)
    var_long_name <- stats::setNames(vapply(meta, `[[`, "", "long_name"), raw_vars)
  }

  return(.new_aeme_output(out_list, model = model, raw = raw_output,
                          var_units = var_units, var_long_name = var_long_name))
}

#' Read Simstrat-AED2 lake water level output
#'
#' @inheritParams read_simstrat_output
#' @returns Data frame with Date and LKE_lvlwtr columns
#' @export
#' @importFrom ncdf4 ncvar_get ncatt_get
read_simstrat_wlev <- function(nc = NULL, file, model = "simstrat_aed2") {
  if (is.null(nc)) {
    nc <- open_nc_safe(file, model = model)
    on.exit(ncdf4::nc_close(nc))
  }
  time_sec <- ncdf4::ncvar_get(nc, "time")
  if (length(time_sec) == 0) {
    cli::cli_abort("No time dimension in Simstrat output")
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
