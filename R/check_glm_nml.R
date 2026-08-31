#' Check GLM nml for common issues
#' 
#' @param file path to GLM nml file
#' @returns Invisibly returns TRUE if no issues found, otherwise aborts with 
#' informative messages
#' @importFrom cli cli_abort
#' @export
check_glm_nml <- function(file) {
  nml <- tryCatch(read_nml(file), error = function(e) {
    cli::cli_abort(c("!" = "Failed to read GLM nml file {.file {file}}.",
                     "x" = e$message))
  })
  base_path <- dirname(file)
  issues <- character()
  
  # --- Helper functions ---
  check_file <- function(path) {
    full <- file.path(base_path, path)
    file.exists(full)
  }
  is_monotonic_increasing <- function(x) all(diff(x) >= 0)
  
  # --- Required sections ---
  required_sections <- c("glm_setup", "morphometry", "time", "meteorology",
                         "light", "sediment")
  missing_sections <- setdiff(required_sections, names(nml))
  if (length(missing_sections) > 0) {
    issues <- c(issues, paste("Missing sections:",
                              paste(missing_sections, collapse = ", ")))
  }
  
  # --- File existence checks ---
  if (!is.null(nml$inflow)) {
    inflow_files  <- strsplit(nml$inflow$inflow_fl, ",")[[1]]
  } else {
    inflow_files <- NULL
  }
  if (!is.null(nml$outflow)) {
    outflow_files <- strsplit(nml$outflow$outflow_fl, ",")[[1]]
  } else {
    outflow_files <- NULL
  }
  
  file_paths <- c(
    nml$meteorology$meteo_fl,
    inflow_files,
    outflow_files,
    nml$wq_setup$wq_nml_file
  )
  
  file_paths <- na.omit(unlist(file_paths))
  missing_files <- file_paths[!vapply(file_paths, check_file, logical(1))]
  if (length(missing_files) > 0) {
    issues <- c(issues,
                paste("Missing input files:", paste(missing_files, 
                                                    collapse = ", ")))
  }
  
  # --- Morphometry checks ---
  morpho <- nml$morphometry
  if (!is.null(morpho)) {
    H <- as.numeric(morpho$H)
    A <- as.numeric(morpho$A)
    bsn_vals <- as.numeric(morpho$bsn_vals)
    
    if (length(H) != bsn_vals) {
      issues <- c(issues, "Number of H values does not match bsn_vals")
    }
    if (length(A) != bsn_vals) {
      issues <- c(issues, "Number of A values does not match bsn_vals")
    }
    
    if (!is_monotonic_increasing(H)) {
      issues <- c(issues, "H is not monotonically increasing")
    }
    if (!is_monotonic_increasing(A)) {
      issues <- c(issues, "A is not monotonically increasing")
    }
    
    if (morpho$latitude < -90 || morpho$latitude > 90) {
      issues <- c(issues, "Latitude out of range (-90 to 90)")
    }
    if (morpho$longitude < -180 || morpho$longitude > 180) {
      issues <- c(issues, "Longitude out of range (-180 to 180)")
    }
  }
  
  # --- Time checks ---
  if (!is.null(nml$time)) {
    start <- as.POSIXct(nml$time$start, tz = "UTC")
    stop  <- as.POSIXct(nml$time$stop, tz = "UTC")
    if (is.na(start) || is.na(stop)) {
      issues <- c(issues, "Could not parse start/stop times")
    } else if (stop <= start) {
      issues <- c(issues, "Stop time must be after start time")
    }
  }
  
  # --- Sediment checks ---
  sed <- nml$sediment
  if (!is.null(sed)) {
    n_zones <- as.numeric(sed$n_zones)
    pars <- c("sed_heat_Ksoil", "sed_temp_depth", "sed_temp_mean",
              "sed_temp_amplitude", "sed_temp_peak_doy", "zone_heights",
              "sed_reflectivity", "sed_roughness")
    # sed_heat_Ksoil and sed_temp_depth only feed GLM's analytical
    # sediment-heat model, enabled via sed_heat_model = 1 (a newer GLM
    # option; older nmls that don't set it at all still rely on that
    # model implicitly, so the check only backs off when sed_heat_model
    # is explicitly present and set to something other than 1)
    heat_model_only_pars <- c("sed_heat_Ksoil", "sed_temp_depth")
    sed_heat_model <- suppressWarnings(as.numeric(sed$sed_heat_model))
    heat_model_disabled <- !is.null(sed$sed_heat_model) &&
      !is.na(sed_heat_model) && sed_heat_model != 1
    for (p in pars) {
      if (p %in% heat_model_only_pars && heat_model_disabled) next
      vals <- as.numeric(sed[[p]])
      if (length(vals) != n_zones) {
        issues <- c(issues, paste0("Number of ", p, " values (", length(vals),
                                   ") does not match n_zones (", n_zones, ")"))
      }
    }

    # sed_heat_model = 2 (dynamic soil-column solver) is supplied by the WQ
    # library, so GLM aborts with it enabled when no WQ module is active.
    if (!is.null(sed$sed_heat_model) && !is.na(sed_heat_model) &&
        sed_heat_model == 2) {
      wq_lib <- tolower(as.character(nml$wq_setup$wq_lib))
      wq_active <- length(wq_lib) == 1 &&
        wq_lib %in% c("aed", "aed2", "api", "fabm")
      if (!wq_active) {
        issues <- c(issues, paste0("sed_heat_model = 2 requires an active WQ ",
                                   "module (&wq_setup with wq_lib = 'aed'/",
                                   "'api'); none found"))
      }
    }

    # When a WQ library is coupled, AEME builds aed_sed_const2d with the same
    # sediment-zone count as GLM and every zone active. A deliberately
    # different / partial set-up is legitimate, so flag mismatches as a
    # warning rather than a hard failure.
    wq_file <- nml$wq_setup$wq_nml_file
    if (!is.null(wq_file) && check_file(wq_file)) {
      aed_nml <- tryCatch(read_nml(file.path(base_path, wq_file)),
                          error = function(e) NULL)
      scd <- aed_nml[["aed_sed_const2d"]]
      if (!is.null(scd) && !is.null(scd$n_zones) && !is.na(n_zones)) {
        aed_nz <- suppressWarnings(as.numeric(scd$n_zones))
        if (!is.na(aed_nz) && aed_nz != n_zones) {
          cli::cli_warn(c(
            "!" = "AED {.field aed_sed_const2d} n_zones ({aed_nz}) does not \\
                   match GLM {.field sediment} n_zones ({n_zones}).",
            "i" = "AEME normally keeps these aligned; check this is intended."
          ))
        }
        az <- suppressWarnings(as.numeric(scd$active_zones))
        if (!is.na(aed_nz) &&
            !isTRUE(all.equal(sort(az), seq_len(aed_nz)))) {
          cli::cli_warn(c(
            "!" = "AED {.field aed_sed_const2d} active_zones \\
                   ({paste(az, collapse = ', ')}) is not all zones \\
                   1..{aed_nz}.",
            "i" = "AEME switches on every sediment zone by default."
          ))
        }
      }
    }
  }
  
  # --- Light checks ---
  light <- nml$light
  if (!is.null(light)) {
    n_bands <- as.numeric(light$n_bands)
    if (length(as.numeric(light$light_extc)) != n_bands) {
      issues <- c(issues, "Number of light_extc values does not match n_bands")
    }
    if (length(as.numeric(light$energy_frac)) != n_bands) {
      issues <- c(issues, "Number of energy_frac values does not match n_bands")
    }
  }
  
  # --- Mixing parameter ranges ---
  mix <- nml$mixing
  if (!is.null(mix)) {
    for (nm in names(mix)) {
      val <- suppressWarnings(as.numeric(mix[[nm]]))
      if (!is.na(val) && val < 0) issues <- c(issues,
                                              paste("Mixing parameter", nm,
                                                    "is negative"))
    }
  }
  
  # --- Output ---
  if (length(issues) == 0) {
    cli_inform_safe(
      c("v" = "GLM nml validation completed - no issues detected.")
    )
    return(invisible(TRUE))
  } else {
    cli::cli_abort(
      c("!" = "Issues found in GLM nml file {.file {file}}:",
        setNames(issues, rep("x", length(issues)))
        ),
      class = "aeme_error_glm_nml"
    )
  }
}
