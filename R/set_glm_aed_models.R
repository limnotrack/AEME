#' Set GLM-AED Models
#' 
#' Set the biogeochemical models to be used in a GLM-AED configuration file.
#'
#' @inheritParams build_aeme
#' @param aed_models Character vector of GLM-AED models to include. Default includes
#' all available AED models: "aed_sedflux", "aed_oxygen", "aed_silica",
#' "aed_nitrogen", "aed_phosphorus", "aed_organic_matter", "aed_phytoplankton",
#' "aed_zooplankton", and "aed_macrophyte".
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
  cli_inform_safe(c("v" = msg))
  
  if (write_nml) {
    write_nml(nml, file)
    return(invisible(aeme))
  } else {
    return(nml)
  }
} 
