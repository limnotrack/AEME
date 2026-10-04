#' Set GLM-AED Models
#' 
#' Set the biogeochemical models to be used in a GLM-AED configuration file.
#' When the file is located from `aeme`, the GLM nml is also updated so that the
#' `&init_profiles` initial conditions, `&inflow` `inflow_vars` and
#' `&mass_balance` `balance_vars` only include variables whose AED module is in
#' `aed_models`. When `nml` is supplied only that aed.nml object is modified.
#'
#' @inheritParams build_aeme
#' @param aed_models Character vector of GLM-AED models to include. Default includes
#' all available AED models: "aed_sedflux", "aed_noncohesive", "aed_oxygen",
#' "aed_silica", "aed_nitrogen", "aed_phosphorus", "aed_organic_matter",
#' "aed_phytoplankton", "aed_zooplankton", "aed_macrophyte", and "aed_totals".
#' @param file Path to the GLM-AED configuration file. If NULL, the function
#' will attempt to locate the file based on the provided Aeme object and path.
#' @param nml GLM-AED nml object. If provided, the function will modify this 
#' object directly instead of reading from a file.
#'
#' @returns If `nml` is provided, returns the modified nml object. Otherwise, 
#' returns the input Aeme object with the updated GLM-AED configuration file.
#' @export
#' 
#' @importFrom cli cli_abort
#'

set_glm_aed_models <- function(aeme, path, aed_models = c("aed_sedflux",
                                                          "aed_noncohesive",
                                                          "aed_oxygen",
                                                          "aed_silica",
                                                          "aed_nitrogen", 
                                                          "aed_phosphorus",
                                                          "aed_organic_matter", 
                                                          "aed_phytoplankton", 
                                                          "aed_zooplankton",
                                                          "aed_macrophyte",
                                                          "aed_totals"), 
                               file = NULL, nml = NULL) {
  # Check if aeme is a Aeme object
  aeme <- check_aeme(aeme)
  if (missing(path)) {
    path <- get_aeme_path(aeme)
  }
  path <- check_path(path = path, must_exist = TRUE)
  
  # Check if aed_models is a character vector
  if (!is.character(aed_models)) {
    cli::cli_abort("{.arg aed_models} must be a character vector.")
  }
  
  if (is.null(nml)) {
    write_nml <- TRUE
    if (is.null(file)) {
      if (missing(aeme)) {
        cli::cli_abort("Either {.arg aeme}, {.arg file} or  {.arg nml} must be 
                       provided.")
      } else {
        if (missing(path)) {
          cli::cli_abort("If {.arg aeme} is provided, then {.arg path} must also 
                         be provided.")
        }
        cfg_files <- get_model_config_files(aeme = aeme, model = "glm_aed", 
                                            path = path)[["glm_aed"]]
        glm_bgc_models <- names(cfg_files)
        glm_bgc_model <- glm_bgc_models[grepl("^aed$", glm_bgc_models)]
        if (length(glm_bgc_model) == 0) {
          cli::cli_abort("No glm_aed model configuration files found for the 
                         specified {.arg aeme} at {.arg path}.")
        }
        file <- cfg_files[[glm_bgc_model]]
      }
    }
    nml <- read_nml(file)
  } else {
    write_nml <- FALSE
  }
  nml <- set_aed_models_nml(nml, aed_models)

  if (write_nml) {
    write_nml(nml, file)

    # Keep the GLM nml consistent with the new module set: drop initial
    # conditions, inflow variables and mass-balance variables that belong to
    # modules that are no longer active (and restore them if re-enabled).
    path_glm <- dirname(dirname(file))
    glm_file <- find_glm_nml(path_glm, must_exist = FALSE)
    if (!is.na(glm_file)) {
      glm_nml <- read_nml(glm_file)
      model_controls <- configuration(aeme)[["model_controls"]]
      glm_nml <- sync_glm_aed_vars(glm_nml, model_controls = model_controls,
                                   aed_models = aed_models,
                                   path_glm = path_glm)
      write_nml(glm_nml, glm_file)
    }
    return(invisible(aeme))
  } else {
    return(nml)
  }
} 

#' Set the active AED modules on an aed.nml object
#'
#' Pure counterpart of [set_glm_aed_models()]: sets `&aed_models` `models` on
#' an aed nml object and returns it, reading and writing no files. The list is
#' set exactly as given; use `resolve_aed_active_modules()` first to add the
#' modules a requested module depends on.
#'
#' @param nml aed nml object (as read by [read_nml()]).
#' @param aed_models character; the AED modules to activate.
#' @return `nml` with `aed_models$models` set.
#' @noRd
set_aed_models_nml <- function(nml, aed_models) {
  if (!is.character(aed_models)) {
    cli::cli_abort("{.arg aed_models} must be a character vector.")
  }
  if (is.null(nml[["aed_models"]])) {
    cli::cli_abort("No {.code aed_models} section found in the provided 
                   configuration file.")
  }
  old_models <- nml[["aed_models"]][["models"]]
  nml[["aed_models"]][["models"]] <- aed_models
  msg <- paste0("Updated GLM-AED models from: ",
                paste(old_models, collapse = ", "),
                " to: ",
                paste(aed_models, collapse = ", "))
  diff_models <- setdiff(old_models, aed_models)
  if (length(diff_models) > 0) {
    cli_inform_safe(c("v" = msg))
  }
  nml
}
