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
#' @param phyto_pars Data frame with phytoplankton parameters from AED.
#' @param load_all logical; also load every other variable present in the
#'   netCDF file, beyond the declared `vars_sim` set. Each such variable is
#'   keyed by its AEME `var_aeme` name if [key_naming] has a translation for
#'   it, or by its raw GLM/netCDF name otherwise. Variables shaped like
#'   GLM's usual `(time)` or `(z, time)` output are loaded the same way as
#'   any declared variable; variables with other dimensions (e.g. `nzones`,
#'   `particle`, `sed_layers`, `lon`, `lat`) are loaded as a
#'   [new_grouped_var()] object instead of being forced into the depth x
#'   time convention. Default `TRUE`.
#'
#' @returns List with AEME output variables
#' @export
#'
#' @importFrom ncdf4 ncvar_get ncatt_get
#' @importFrom lubridate hour
#' @importFrom dplyr filter mutate pull

read_glm_output <- function(nc = NULL, vars_sim = NULL, depths = NULL,
                            dates = NULL, date_index = NULL, incl_fluxes = TRUE,
                            output_hour = 0, file, phyto_pars = NULL,
                            load_all = TRUE) {

  if (is.null(nc)) {
    nc <- open_nc_safe(file, model = "glm_aed")
    on.exit(ncdf4::nc_close(nc))
  }
  out_list <- list()
  # glm DOES NOT output initial profiles
  hours_since  <- ncdf4::ncvar_get(nc, "time")
  if (length(hours_since) == 0) {
    out <- empty_model_output(reason = "Empty time dimension")
    return(out)
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
  if (length(glm_dates) < max(date_index)) {
    cli::cli_alert_warning("date_index exceeds available GLM output dates. 
                          Returning empty output.")
    out <- empty_model_output(
      reason = "date_index exceeds available GLM output dates"
    )
    return(out)
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
    overflow <- ncdf4::ncvar_get(nc, "overflow_vol")[date_index]
    out_list[["LKE_overflow"]] <- overflow
    tot_outflow <- ncdf4::ncvar_get(nc, "tot_outflow_vol")[date_index]
    out_list[["LKE_outflow"]] <- tot_outflow
    out_list[["LKE_outftot"]] <- tot_outflow + overflow
    out_list[["LKE_precip"]] <- ncdf4::ncvar_get(nc, "precipitation")[date_index]
    out_list[["LKE_pcpvol"]] <- out_list[["LKE_precip"]] * out_list[["LKE_A0"]]
    out_list[["HYD_surft"]] <- ncdf4::ncvar_get(nc, "surface_temp")[date_index]
  }
  
  if ("LKE_photic" %in% vars_sim | "LKE_efold" %in% vars_sim) {
    rad <- ncdf4::ncvar_get(nc, "radn")[, date_index]
    rad <- interp_static_grid(var = rad,
                              midpoints = midpoints,
                              out_depths = out_depths)
    
    suppressWarnings({
      out_list[["LKE_efold"]] <- sapply(seq_len(ncol(rad)), \(t) {
        ok <- complete.cases(rad[, t], out_depths[, t])
        if (sum(ok) < 2 || length(unique(rad[ok, t])) < 2) return(NA)
        ref_rad <- (1/exp(1) * max(rad[, t]))
        approx(rad[, t], out_depths[, t], xout = ref_rad)$y
      })
      out_list[["LKE_photic"]] <- sapply(seq_len(ncol(rad)), \(t) {
        ok <- complete.cases(rad[, t], out_depths[, t])
        if (sum(ok) < 2 || length(unique(rad[ok, t])) < 2) return(NA)
        ref_rad <- (0.01 * max(rad[, t]))
        approx(rad[, t], out_depths[, t], xout = ref_rad)$y
      })
    })
    vars_sim <- vars_sim[!vars_sim %in% c("LKE_photic", "LKE_efold")]
  }
  
  out_list <- lapply(out_list, as.vector)
  out_list[["Date"]] <- dates
  
  
  # Add depths as a matrix
  out_list[["LKE_depths"]] <- out_depths
  
  
  if (!is.null(vars_sim)) {
    model_vars <- get_model_vars(vars_sim = vars_sim, model = "glm_aed")
    model_vars_vec <- get_model_vars(vars_sim = vars_sim, model = "glm_aed", 
                                     as_vector = TRUE)
    
    nc_vars <- names(nc$var)
    vars_chk <- data.frame(
      vars = model_vars_vec,
      present = model_vars_vec %in% nc_vars
      ) |> 
      dplyr::left_join(model_vars, by = c("vars" = "glm_aed")) |> 
      dplyr::rename(conv_factor = conversion_aed)
    
    if (any(grepl("PHY", model_vars_vec))) {
      phyto_vars <- model_vars_vec[grepl("PHY", model_vars_vec)]
      phyto_vars <- phyto_vars[phyto_vars != "PHY_tchla"]
      phyto_vars <- gsub("PHY_", "", phyto_vars)
      if (!is.null(phyto_pars)) {
        Xcc <- phyto_pars |> 
          dplyr::filter(p_name == "Xcc") 
        for (pv in phyto_vars) {
          vars_chk$conv_factor[vars_chk$vars == paste0("PHY_", pv)] <- 12.0 / Xcc[[pv]]
        }
      }
    }
    
    out_vars <- lapply(model_vars_vec, \(v) {
      if(vars_chk$present[vars_chk$vars == v] == FALSE) {
        # cli::cli_alert_warning("Variable {.val {v}} not found in GLM output.
        #                        Returning NULL for this variable.")
        return(NULL)
      }
      conv_factor <- vars_chk$conv_factor[vars_chk$vars == v]
      if (is.na(conv_factor)) {
        conv_factor <- 1
      }
      var_out <- ncdf4::ncvar_get(nc, v)
      if (grepl("_Z", v)) {
        return(var_out)
      }
      if (length(dim(var_out)) == 3) {
        var_out <- var_out[, , date_index, drop = FALSE]
      } else if (length(dim(var_out)) == 2) {
        var <- var_out[, date_index, drop = FALSE]  * conv_factor
        out <- interp_static_grid(var = var,
                                  midpoints = midpoints,
                                  out_depths = out_depths)
        return(out)
      } else if (length(dim(var_out)) == 1) {
        # ncdf4::ncvar_get() can return a single-dimension variable with a
        # length-1 dim attribute still attached (class "array", not a
        # plain vector), which survives `[` indexing -- strip it so
        # downstream code's is.null(dim(x)) checks correctly treat this as
        # an ordinary 1D time series (matches the as.vector() normalisation
        # already applied to the fixed-name flux variables above)
        var_out <- as.vector(var_out[date_index] * conv_factor)
        return(var_out)
      } else {
        cli::cli_abort(paste("Variable", v, "has unsupported number of dimensions"))
      }
    })
    
    out_list <- c(out_list, out_vars)
  }

  # ---- Load every remaining variable present in the file ----
  # Variables already handled above (by the fixed-name blocks and, if
  # vars_sim was supplied, the declared/translated loop) are skipped;
  # everything else in the file is loaded too -- keyed by its var_aeme
  # name if key_naming has a translation, otherwise by its raw GLM name.
  if (isTRUE(load_all)) {
    already_extracted <- c("time", "z", "lake_level")
    if (incl_fluxes) {
      already_extracted <- c(
        already_extracted,
        "daily_qe", "daily_qh", "daily_qlw", "daily_qsw", "lake_volume",
        "evaporation", "evap_mass_flux", "surface_area", "tot_inflow_vol",
        "overflow_vol", "tot_outflow_vol", "precipitation", "surface_temp"
      )
    }
    if ("LKE_photic" %in% vars_sim | "LKE_efold" %in% vars_sim) {
      already_extracted <- c(already_extracted, "radn")
    }
    if (!is.null(vars_sim)) {
      already_extracted <- c(already_extracted, model_vars_vec)
    }

    data("key_naming", package = "AEME", envir = environment())
    glm_to_var_aeme <- stats::setNames(key_naming$var_aeme, key_naming$glm_aed)

    nc_vars <- names(nc$var)
    remaining_vars <- setdiff(nc_vars, already_extracted)

    for (v in remaining_vars) {
      key <- unname(glm_to_var_aeme[v])
      if (is.na(key) || !nzchar(key)) key <- v
      if (key %in% names(out_list)) next

      conv_idx <- match(v, key_naming$glm_aed)
      conv_factor <- if (!is.na(conv_idx)) key_naming$conversion_aed[conv_idx] else NA
      if (is.na(conv_factor)) conv_factor <- 1

      result <- tryCatch({
        # ncdf4::ncvar_get() drops length-1 dimensions by default (e.g. a
        # single-column GLM run's degenerate lon/lat), so the array it
        # actually returns won't have those axes even though the file's
        # dimension metadata still lists them for every variable -- match
        # that behaviour here rather than comparing against the raw
        # (pre-squeeze) dimension list
        dim_objs  <- Filter(\(d) d$len > 1, nc$var[[v]]$dim)
        dim_names <- vapply(dim_objs, \(d) d$name, character(1))

        # Guard against accidentally loading a genuinely huge variable in
        # full (e.g. a real particle-tracking run, where `particle` can be
        # declared with length in the millions even though it's unused --
        # length 1 -- in most runs)
        n_elem <- prod(vapply(dim_objs, \(d) d$len, numeric(1)))
        if (length(n_elem) == 0) n_elem <- 1

        if (n_elem > 5e6) {
          cli::cli_warn(c("!" = "Skipping variable {.val {v}}: {n_elem} values ({paste(dim_names, collapse = ' x ')}) is too large to load automatically."))
          NULL
        } else if (setequal(dim_names, "time")) {
          # See the analogous as.vector() fix above -- a single-dimension
          # netCDF variable can come back with a length-1 dim attribute
          # still attached, which survives `[` indexing
          var_out <- ncdf4::ncvar_get(nc, v)
          as.vector(var_out[date_index] * conv_factor)
        } else if (setequal(dim_names, c("z", "time"))) {
          var_out <- ncdf4::ncvar_get(nc, v)
          if (dim_names[1] != "z") var_out <- t(var_out)
          var <- var_out[, date_index, drop = FALSE] * conv_factor
          interp_static_grid(var = var, midpoints = midpoints,
                             out_depths = out_depths)
        } else {
          .read_glm_grouped_var(nc = nc, v = v, dim_objs = dim_objs,
                                dim_names = dim_names, date_index = date_index,
                                dates = dates)
        }
      }, error = function(e) {
        cli::cli_warn(c("!" = "Could not read variable {.val {v}} from GLM output: {conditionMessage(e)}"))
        NULL
      })

      if (!is.null(result)) {
        out_list[[key]] <- result
      }
    }
  }

  out_list <- c(out_list, list(ok = TRUE, reason = NULL))
  return(out_list)
}

#' Read a GLM-AED output variable with dimensions other than `(time)` or
#' `(z, time)` into a [new_grouped_var()], preserving each dimension's
#' actual coordinate/index values
#' @param nc open ncdf4 object.
#' @param v character; the netCDF variable name.
#' @param dim_objs list; `nc$var[[v]]$dim`.
#' @param dim_names character; names of `dim_objs`, in order.
#' @inheritParams read_glm_output
#' @param dates Date vector; already resolved simulation dates
#'   (`glm_dates[date_index]`), used as the coordinate values for a `"time"`
#'   dimension.
#' @return An `aeme_grouped_var` object.
#' @noRd
.read_glm_grouped_var <- function(nc, v, dim_objs, dim_names, date_index, dates) {
  arr <- ncdf4::ncvar_get(nc, v)
  if (is.null(dim(arr)) && length(dim_names) == 1) {
    dim(arr) <- length(arr)
  }

  time_pos <- which(dim_names == "time")
  if (length(time_pos) == 1) {
    d <- dim(arr)
    idx_list <- lapply(d, seq_len)
    idx_list[[time_pos]] <- date_index
    arr <- do.call(`[`, c(list(arr), idx_list, list(drop = FALSE)))
  }

  dim_values <- stats::setNames(
    lapply(dim_objs, \(d) {
      if (d$name == "time") return(dates)
      vals <- d$vals
      if (is.null(vals) || length(vals) != d$len) vals <- seq_len(d$len)
      vals
    }),
    dim_names
  )

  new_grouped_var(value = arr, dim_names = dim_names, dim_values = dim_values)
}

#' Read GLM lake water level output
#' 
#' @inheritParams read_glm_output
#' @returns Data frame with Date and LKE_lvlwtr columns
#' @export
#' @importFrom ncdf4 ncvar_get ncatt_get
read_glm_wlev <- function(nc = NULL, file) {
  if (is.null(nc)) {
    nc <- open_nc_safe(file, model = "glm_aed")
    on.exit(ncdf4::nc_close(nc))
  }
  hours_since  <- ncdf4::ncvar_get(nc, "time")
  if (length(hours_since) == 0) {
    cli::cli_abort("No time dimension in GLM output")
  }
  date_start <- as.POSIXct(gsub("hours since ", "",
                                ncdf4::ncatt_get(nc,'time','units')$value))
  glm_dates <- as.POSIXct(hours_since * 3600 + date_start) |> 
    as.Date()
  
  lake_level <- ncdf4::ncvar_get(nc, "lake_level")
  
  out <- data.frame(Date = glm_dates,
                    LKE_lvlwtr = lake_level)
  return(out)
}
