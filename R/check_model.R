#' Check model name and return standardized code
#'
#' @param model Character vector of model names. Valid options are:
#'  "DYRESM-CAEDYM", "GLM-AED", "GOTM-WET" or their corresponding codes
#'  "dy_cd", "glm_aed", "gotm_wet".
#'  
#'  @importFrom cli cli_abort
#'
#' @returns Character vector of standardized model codes.
#' @export
#'
#' @examples
#' check_model(c("GLM-AED", "gotm_wet"))

check_model <- function(model) {
  valid_models <- c(
    "DYRESM-CAEDYM" = "dy_cd",
    "GLM-AED"       = "glm_aed",
    "GOTM-WET"      = "gotm_wet"
  )
  
  valid_names <- c(names(valid_models), unname(valid_models))
  
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
  
  # ---- Canonical named output ----
  
  # Step 1: map everything to short codes
  short_codes <- ifelse(
    model %in% names(valid_models),
    valid_models[model],
    model
  )
  
  # Step 2: rebuild named vector (full name -> short code)
  mapped <- setNames(
    short_codes,
    names(valid_models)[match(short_codes, valid_models)]
  )
  
  return(mapped)
}

