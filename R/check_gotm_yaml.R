#' Check GOTM YAML configuration file for common issues
#'
#' @param file Path to GOTM YAML configuration file
#'
#' @returns Invisibly returns TRUE if no issues are found; otherwise, throws an 
#' error
#' @export
#' @importFrom yaml read_yaml
#' @importFrom cli cli_abort

check_gotm_yaml <- function(file) {
  base_path <- dirname(file)
  file <- basename(file)
  normalized_file <- normalizePath(file.path(base_path, file), mustWork = TRUE)
  # --- Read file ---
  gotm <- tryCatch(
    yaml::read_yaml(normalized_file),
    error = function(e) {
      cli::cli_abort(c("!" = "Failed to read GOTM YAML file {.file {file}}.",
                       "x" = e$message))
    }
  )
  
  issues <- character()
  
  # --- Location checks ---
  loc <- gotm$location
  if (is.null(loc$name)) issues <- c(issues, "Missing location name.")
  lat <- loc$latitude %||% NA
  lon <- loc$longitude %||% NA
  depth <- loc$depth %||% NA
  
  if (is.na(lat) || lat < -90 || lat > 90)
    issues <- c(issues, "Latitude out of valid range (-90 to 90).")
  if (is.na(lon) || lon < -360 || lon > 360)
    issues <- c(issues, "Longitude out of valid range (-360 to 360).")
  if (is.na(depth) || depth <= 0)
    issues <- c(issues, "Depth must be greater than 0.")
  
  # --- Time settings ---
  time <- gotm$time
  if (is.null(time$start) || is.null(time$stop)) {
    issues <- c(issues, "Missing start or stop time in 'time' section.")
  } else {
    start <- as.POSIXct(time$start, tz = "UTC", tryFormats = c("%Y-%m-%d %H:%M:%S"))
    stop  <- as.POSIXct(time$stop, tz = "UTC", tryFormats = c("%Y-%m-%d %H:%M:%S"))
    if (is.na(start) || is.na(stop))
      issues <- c(issues, "Start or stop date could not be parsed.")
    else if (stop <= start)
      issues <- c(issues, "Stop time must be after start time.")
  }
  if (is.null(time$dt) || time$dt <= 0)
    issues <- c(issues, "Invalid time step (dt must be > 0).")
  
  # --- Grid settings ---
  grid <- gotm$grid
  if (is.null(grid$nlev) || grid$nlev < 1)
    issues <- c(issues, "Number of grid levels (nlev) must be ≥ 1.")
  if (!is.null(grid$ddu) && grid$ddu < 0)
    issues <- c(issues, "Surface zooming (ddu) must be ≥ 0.")
  if (!is.null(grid$ddl) && grid$ddl < 0)
    issues <- c(issues, "Bottom zooming (ddl) must be ≥ 0.")
  
  # --- Meteorology ---
  meteo <- gotm$surface$meteo
  if (is.null(meteo)) {
    issues <- c(issues, "Missing 'surface/meteo' section.")
  } else {
    required_vars <- c("u10", "v10", "airp", "airt", "hum", "cloud")
    missing_vars <- required_vars[!required_vars %in% names(meteo)]
    if (length(missing_vars) > 0)
      issues <- c(issues, sprintf("Missing meteorological variables: %s",
                                  paste(missing_vars, collapse = ", ")))
    met_names <- c("u10", "v10", "airp", "airt", "hum", "cloud", "swr",
                   "precip")
    for (nm in met_names) {
      entry <- meteo[[nm]]
      method <- entry$method %||% NA
      if (is.na(method)) {
        issues <- c(issues, sprintf("Missing 'method' for meteo variable '%s'.", nm))
      } else if (method == 2) {
        file_path <- entry$file %||% ""
        if (file_path == "" || !file.exists(file.path(base_path, file_path)))
          issues <- c(issues, sprintf("Meteo variable '%s' (method=2) requires valid file path.", nm))
      }
    }
  }
  
  # --- Temperature forcing ---
  temp <- gotm$temperature
  if (is.null(temp)) {
    issues <- c(issues, "Missing 'temperature' section.")
  } else {
    method <- temp$method %||% NA
    if (is.na(method))
      issues <- c(issues, "Missing 'method' in 'temperature' section.")
    else if (method == 2) {
      file_path <- temp$file %||% ""
      if (file_path == "" || !file.exists(file.path(base_path, file_path)))
        issues <- c(issues, "Temperature forcing method=2 requires valid file path.")
    }
  }
  
  # --- Salinity forcing ---
  sal <- gotm$salinity
  if (is.null(sal)) {
    issues <- c(issues, "Missing 'salinity' section.")
  } else {
    method <- sal$method %||% NA
    if (is.na(method))
      issues <- c(issues, "Missing 'method' in 'salinity' section.")
    else if (method == 2) {
      file_path <- sal$file %||% ""
      if (file_path == "" || !file.exists(file.path(base_path, file_path)))
        issues <- c(issues, "Salinity forcing method=2 requires valid file path.")
    }
  }
  
  # --- Light extinction ---
  light <- gotm$light_extinction
  if (is.null(light)) {
    issues <- c(issues, "Missing 'light_extinction' section.")
  } else {
    method <- light$method %||% NA
    if (is.na(method))
      issues <- c(issues, "Missing 'method' in 'light_extinction' section.")
    if (method == 7) {
      if (is.null(light$A$constant_value) || light$A$constant_value < 0 || 
          light$A$constant_value > 1)
        issues <- c(issues, "Light extinction A.constant_value must be between 0 and 1.")
      if (is.null(light$g1$constant_value) || light$g1$constant_value <= 0)
        issues <- c(issues, "Light extinction g1.constant_value must be > 0.")
      if (is.null(light$g2$constant_value) || light$g2$constant_value <= 0)
        issues <- c(issues, "Light extinction g2.constant_value must be > 0.")
    }
  }
  
  # --- Bottom roughness ---
  bottom <- gotm$bottom
  if (is.null(bottom)) {
    issues <- c(issues, "Missing 'bottom' section.")
  } else if (is.null(bottom$h0b) || bottom$h0b < 0) {
    issues <- c(issues, "Bottom roughness (h0b) must be ≥ 0.")
  }
  
  # --- Streams checks (multiple streams) ---
  streams <- gotm$streams
  if (!is.null(streams)) {
    for (stream_name in names(streams)) {
      inflow <- streams[[stream_name]]
      if (!is.null(inflow)) {
        
        # --- Check stream-level numeric parameters ---
        issues <- c(
          issues,
          check_range(inflow$zu, paste0("streams$", stream_name, "$zu"), -1e3, 1e3),
          check_range(inflow$zl, paste0("streams$", stream_name, "$zl"), -1e3, 1e3),
          check_range(inflow$method, paste0("streams$", stream_name, "$method"), 0, 10)
        )
        
        # --- Loop over flow, temp, salt, nutrients, etc. ---
        inflow_vars <- setdiff(names(inflow), c("method", "zu", "zl"))
        for (v in inflow_vars) {
          sub <- inflow[[v]]
          if (is.list(sub)) {
            # File existence if method == 2
            if (!is.null(sub$method) && as.numeric(sub$method) == 2 && !is.null(sub$file)) {
              issues <- c(issues, check_file_if_exists(sub$file, base_path))
            }
            # Column index sanity check
            if (!is.null(sub$column) && as.numeric(sub$column) < 1) {
              issues <- c(issues, paste0("streams$", stream_name, "$", v, " column index must be >= 1"))
            }
            # Optional: check numeric fields
            numeric_fields <- c("constant_value", "scale_factor", "offset")
            for (f in numeric_fields) {
              if (!is.null(sub[[f]])) {
                issues <- c(issues, check_range(sub[[f]], paste0("streams$", stream_name, "$", v, "$", f)))
              }
            }
          }
        }
      }
    }
  }
  
  
  # --- FABM checks ---
  fabm <- gotm$fabm
  if (!is.null(fabm)) {
    issues <- c(
      issues,
      check_logical(fabm$use, "fabm$use"),
      check_logical(fabm$freshwater_impact, "fabm$freshwater_impact")
    )
    
    # Check nested feedback flags
    fb <- fabm$feedbacks
    if (!is.null(fb)) {
      for (nm in names(fb)) {
        issues <- c(issues, check_logical(fb[[nm]], paste("fabm$feedbacks$", nm)))
      }
    }
    
    num <- fabm$numerics
    if (!is.null(num)) {
      issues <- c(
        issues,
        check_range(num$split_factor, "fabm$numerics$split_factor", 1, 100),
        check_range(num$cnpar, "fabm$numerics$cnpar", 0, 1)
      )
    }
  }
  
  # --- Physical constants ---
  phys <- gotm$physical_constants
  if (!is.null(phys)) {
    issues <- c(
      issues,
      check_range(phys$gravity, "physical_constants$gravity", 9.8, 10),
      check_range(phys$rho_0, "physical_constants$rho_0", 0, 1200),
      check_range(phys$cp, "physical_constants$cp", 0, 10000)
    )
  }
  
  # --- Buoyancy ---
  buoy <- gotm$buoyancy
  if (!is.null(buoy)) {
    issues <- c(
      issues,
      check_range(buoy$surf_ini, "buoyancy$surf_ini", -10, 10),
      check_range(buoy$NN_ini, "buoyancy$NN_ini", 0, 1e-3)
    )
  }
  
  # --- Equation of state ---
  eq <- gotm$eq_state
  if (!is.null(eq)) {
    issues <- c(
      issues,
      check_range(eq$T0, "eq_state$T0", -2, 40),
      check_range(eq$S0, "eq_state$S0", 0, 40)
    )
  }
  
  # --- Restart ---
  rst <- gotm$restart
  if (!is.null(rst)) {
    issues <- c(
      issues,
      check_logical(rst$load, "restart$load"),
      check_logical(rst$allow_missing_variable, "restart$allow_missing_variable")
    )
  }
  
  # --- Final messages ---
  if (length(issues) == 0) {
    cli_inform_safe(
      c("v" = "GOTM YAML validation completed — no issues detected.")
    )
    return(invisible(TRUE))
  } else {
    cli::cli_abort(
      c(
        "!" = "Issues found in GOTM YAML file {.file {normalized_file}}:",
        setNames(issues, rep("x", length(issues)))
      ),
      class = "aeme_error_gotm_yaml"
    )
  }
}
