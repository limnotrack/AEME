#' Check Simstrat par file for common issues
#'
#' @param file path to Simstrat `.par` (JSON) file
#' @returns Invisibly returns TRUE if no issues found, otherwise aborts with
#' informative messages
#' @importFrom cli cli_abort
#' @export
check_simstrat_par <- function(file) {

  par <- tryCatch(jsonlite::fromJSON(file, simplifyVector = FALSE),
                  error = function(e) {
                    cli::cli_abort(c("!" = "Failed to read Simstrat par file {.file {file}}.",
                                     "x" = e$message))
                  })
  base_path <- dirname(file)
  issues <- character()

  check_file <- function(path) {
    if (is.null(path) || path == "") return(FALSE)
    file.exists(file.path(base_path, path))
  }

  # BGC config section is "AED2Config" for Simstrat-AED2, "AEDConfig" for
  # Simstrat-AED -- accept whichever is present rather than assuming
  # AED2Config, so this validator works for either coupling.
  bgc_tag <- if ("AEDConfig" %in% names(par)) "AED" else "AED2"
  bgc_section <- paste0(bgc_tag, "Config")

  # --- Required sections ---
  required_sections <- c("Input", "Output", "ModelConfig",
                         "Simulation", "ModelParameters")
  missing_sections <- setdiff(required_sections, names(par))
  if (!any(c("AEDConfig", "AED2Config") %in% names(par))) {
    missing_sections <- c(missing_sections, "AEDConfig or AED2Config")
  }
  if (length(missing_sections) > 0) {
    issues <- c(issues, paste("Missing sections:",
                              paste(missing_sections, collapse = ", ")))
  }

  # --- Input file existence checks ---
  input <- par[["Input"]]
  if (!is.null(input)) {
    input_files <- unlist(input)
    missing_files <- input_files[!vapply(input_files, check_file, logical(1))]
    if (length(missing_files) > 0) {
      issues <- c(issues,
                  paste("Missing input files:", paste(missing_files,
                                                      collapse = ", ")))
    }
  }

  # --- AED/AED2 checks ---
  aed_cfg <- par[[bgc_section]]
  model_cfg <- par[["ModelConfig"]]
  couple_key <- paste0("Couple", bgc_tag)
  if (!is.null(aed_cfg) && isTRUE(model_cfg[[couple_key]])) {
    if (!check_file(aed_cfg[[paste0(bgc_tag, "ConfigFile")]])) {
      issues <- c(issues, paste0(bgc_tag, " config file not found: ",
                                 aed_cfg[[paste0(bgc_tag, "ConfigFile")]]))
    }
    for (nm in c(paste0("Path", bgc_tag, "initial"), paste0("Path", bgc_tag, "inflow"))) {
      path <- aed_cfg[[nm]]
      if (is.null(path) || !dir.exists(file.path(base_path, path))) {
        issues <- c(issues, paste0(nm, " directory not found: ", path))
      }
    }
  }

  # --- Output checks ---
  output <- par[["Output"]]
  if (!is.null(output)) {
    if (isTRUE(model_cfg[[couple_key]]) &&
        !"WaterH" %in% unlist(output[["Variables"]])) {
      issues <- c(issues,
                  "Output.Variables should include \"WaterH\" so AEME can
                  derive the lake water level (LKE_lvlwtr) from the output.")
    }
  }

  # --- Simulation / time checks ---
  sim <- par[["Simulation"]]
  if (!is.null(sim)) {
    start_d <- suppressWarnings(as.numeric(sim[["Start d"]]))
    end_d   <- suppressWarnings(as.numeric(sim[["End d"]]))
    if (is.na(start_d) || is.na(end_d)) {
      issues <- c(issues, "Could not parse Simulation.Start d / End d")
    } else if (end_d <= start_d) {
      issues <- c(issues, "Simulation.End d must be after Simulation.Start d")
    }
    ts <- suppressWarnings(as.numeric(sim[["Timestep s"]]))
    if (is.na(ts) || ts <= 0) {
      issues <- c(issues, "Simulation.Timestep s must be a positive number")
    } else {
      times <- suppressWarnings(as.numeric(output[["Times"]]))
      if (!is.na(times) && abs(times * ts - 86400) > 1e-6) {
        issues <- c(issues,
                    "Output.Times * Simulation.Timestep s must equal 86400
                    (exactly one output row per day) -- AEME's get_date_index()
                    indexes model output positionally by day, not by matching
                    actual dates.")
      }
    }
  }

  # --- ModelParameters range checks ---
  mp <- par[["ModelParameters"]]
  if (!is.null(mp)) {
    lat <- suppressWarnings(as.numeric(mp[["lat"]]))
    if (!is.na(lat) && (lat < -90 || lat > 90)) {
      issues <- c(issues, "ModelParameters.lat out of range (-90 to 90)")
    }
  }

  # --- Output ---
  if (length(issues) == 0) {
    cli_inform_safe(
      c("v" = "Simstrat par validation completed - no issues detected.")
    )
    return(invisible(TRUE))
  } else {
    cli::cli_abort(
      c("!" = "Issues found in Simstrat par file {.file {file}}:",
        setNames(issues, rep("x", length(issues)))
      ),
      class = "aeme_error_simstrat_par"
    )
  }
}
