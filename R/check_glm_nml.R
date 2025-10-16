#' Check GLM nml for common issues
#' 
#'
#' @param file path to GLM nml file
#'
#' @returns TRUE if no issues found, FALSE otherwise
#' @export
#'

check_glm_nml <- function(file) {
  nml <- read_nml(file)
  base_path <- dirname(file)
  issues <- character()
  
  # --- Helper functions ---
  check_file <- function(path) {
    full <- file.path(base_path, path)
    file.exists(full)
  }
  
  is_monotonic_increasing <- function(x) {
    all(diff(x) >= 0)
  }
  
  # --- Check required sections ---
  required_sections <- c(
    "glm_setup", "morphometry", "time", "meteorology",
    "light", "sediment"
  )
  missing_sections <- setdiff(required_sections, names(nml))
  if (length(missing_sections) > 0) {
    issues <- c(issues, paste("Missing sections:", paste(missing_sections, collapse = ", ")))
  }
  
  # --- File existence checks ---
  inflow_files <- strsplit(nml$inflow$inflow_fl, ",")[[1]]
  outflow_files <- strsplit(nml$outflow$outflow_fl, ",")[[1]]
  file_paths <- c(
    nml$meteorology$meteo_fl,
    inflow_files,
    outflow_files,
    # nml$output$out_dir,
    nml$wq_setup$wq_nml_file
  )
  
  file_paths <- unlist(file_paths)
  file_paths <- file_paths[!is.na(file_paths)]
  
  missing_files <- file_paths[!vapply(file_paths, check_file, logical(1))]
  if (length(missing_files) > 0) {
    issues <- c(issues, paste("Missing input files:", paste(missing_files, collapse = ", ")))
  }
  
  # --- Morphometry checks ---
  morpho <- nml$morphometry
  if (!is.null(morpho)) {
    H <- as.numeric(morpho$H)
    A <- as.numeric(morpho$A)
    
    if (length(H) != morpho$bsn_vals) {
      issues <- c(issues, "Number of H values does not match bsn_vals")
    }
    if (length(A) != morpho$bsn_vals) {
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
  if (!is.null(nml$sediment)) {
    sed <- nml$sediment
    n_zones <- as.numeric(sed$n_zones)
    n_temp_vals <- length(as.numeric(sed$sed_temp_mean))
    
    if (n_temp_vals != n_zones) {
      issues <- c(
        issues,
        paste0("Number of sed_temp_mean values (", n_temp_vals,
               ") does not match n_zones (", n_zones, ")")
      )
    }
  }
  
  # --- Light checks ---
  if (!is.null(nml$light)) {
    light <- nml$light
    n_bands <- as.numeric(light$n_bands)
    if (length(as.numeric(light$light_extc)) != n_bands) {
      issues <- c(issues, "Number of light_extc values does not match n_bands")
    }
    if (length(as.numeric(light$energy_frac)) != n_bands) {
      issues <- c(issues, "Number of energy_frac values does not match n_bands")
    }
  }
  
  # --- Mixing parameter ranges ---
  if (!is.null(nml$mixing)) {
    mix <- nml$mixing
    for (nm in names(mix)) {
      val <- suppressWarnings(as.numeric(mix[[nm]]))
      if (!is.na(val) && val < 0) {
        issues <- c(issues, paste("Mixing parameter", nm, "is negative"))
      }
    }
  }
  
  # --- Output summary ---
  if (length(issues) == 0) {
    message("GLM nml checks passed with no issues.")
    return(invisible(TRUE))
  } else {
    cat("Issues found in GLM nml:\n")
    cat(paste0(" - ", issues), sep = "\n")
    return(invisible(FALSE))
  }
}
