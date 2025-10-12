#' Toggle between model display names and codes
#' 
#' A helper function to switch between user-friendly model names and their 
#' corresponding codes used in the backend.
#'
#' @param model Model name to be toggled.
#' @param to Target format: either "display" for user-friendly names or "code" 
#' for backend codes. If NULL, the function will toggle to the opposite format.
#'
#' @returns The model name in the desired format.
#' @export
#'
#' @examples
#' toggle_models("DYRESM-CAEDYM")  # Returns "dy_cd"
#' toggle_models("dy_cd", to = "display")  # Returns "DYRESM-CAEDYM"
#' toggle_models("GLM-AED")  # Returns "glm_aed"
#' toggle_models("gotm_wet", to = "display")  # Returns "GOTM-WET"
#' toggle_models("GOTM-WET", to = "code")  # Returns "gotm_wet"
#' df <- data.frame(model = rep(c("dy_cd", "glm_aed"), 50))
#' df <- df |>
#'         dplyr::mutate(Model = toggle_models(model, to = "display"))

toggle_models <-   function(model = NULL, to = NULL) {
  models <- c(
    "DYRESM-CAEDYM" = "dy_cd",
    "GLM-AED"       = "glm_aed",
    "GOTM-WET"      = "gotm_wet"
  )
  
  if (all(model %in% names(models))) {
    idx <- match(model, names(models))
    current <- "display"
  } else if (all(model %in% models)) {
    idx <- match(model, models)
    current <- "code"
  } else {
    stop("Model not found in either display names or codes.")
  }
  
  if (is.null(to)) {
    to <- ifelse(current == "display", "code", "display")
  }
  
  if (to == "code") {
    return(models[idx])
  } else if (to == "display") {
    return(names(models)[idx])
  } else {
    stop("Argument 'to' must be either 'display' or 'code'.")
  }
}
