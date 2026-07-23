#' Check model name and return standardized code
#'
#' @param model Character vector of model names. Valid options are:
#'  "DYRESM-CAEDYM", "GLM-AED", "GOTM-WET" or their corresponding codes
#'  "dy_cd", "glm_aed", "gotm_wet".
#'  
#' @importFrom cli cli_abort cli_alert_info
#'
#' @returns Character vector of standardized model codes.
#' @export
#'
#' @examples
#' check_model(c("GLM-AED", "gotm_wet"))

check_model <- function(model, os_valid = FALSE) {
  
  all_models <- c(
    "DYRESM-CAEDYM" = "dy_cd",
    "GLM-AED"       = "glm_aed",
    "GOTM-WET"      = "gotm_wet"
  )
  
  valid_names <- c(names(all_models), unname(all_models))
  
  # Check that model is provided and valid
  if (missing(model) || is.null(model) || !length(model)) {
    cli::cli_abort("{.arg model} must be provided and not be empty.")
  }
  
  invalid <- setdiff(model, valid_names)
  
  if (length(invalid) > 0) {
    cli::cli_abort(
      c(
        "!" = "Invalid model name{?s}: {.val {invalid}}.",
        "i" = "Valid options are: {.val {valid_names}}."
      ),
      class = "aeme_error_model_invalid"
    )
  }
  
  # OS Check
  if (os_valid) {
    os <- .detect_os()
    
    windows_only <- c("dy_cd", "gotm_wet")
    
    os_valid_models <- if (os == "windows") {
      all_models
    } else {
      cli::cli_alert_info("DYRESM-CAEDYM and GOTM-WET are only available on Windows. Defaulting to GLM-AED for {.field {os}} OS.")
      # Check for exe
      exe <- .resolve_glm_exec()
      all_models[!all_models %in% windows_only]
    }
  } else {
    os_valid_models <- all_models
  }
  os_valid_names <- c(names(os_valid_models), unname(os_valid_models))
  model <- model[model %in% os_valid_names]
  
  # ---- Canonical named output ----
  
  # Step 1: map everything to short codes
  short_codes <- ifelse(
    model %in% names(all_models),
    all_models[model],
    model
  )
  
  # Step 2: rebuild named vector (full name -> short code)
  mapped <- setNames(
    short_codes,
    names(all_models)[match(short_codes, all_models)]
  )
  
  return(mapped)
}

