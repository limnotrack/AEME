#' Check AEME variable names
#' 
#' Check if the provided variable names are valid AEME variable names.
#'
#' @param vars Character vector of variable names to check.
#'
#' @returns Invisibly returns TRUE if all variables are valid, otherwise throws 
#' an error.
#' @importFrom cli cli_abort
#' @export
#'
#' @examples
#' check_aeme_vars("HYD_temp")

check_aeme_vars <- function(vars) {
  # Load key dataset
  utils::data("key_naming", package = "AEME", envir = environment())
  valid_vars <- key_naming$name  # column with valid variable names
  
  # Validate
  is_valid <- vars %in% valid_vars
  if (all(is_valid)) {
    return(vars)
  }
  
  # Identify invalid variables
  invalid <- vars[!is_valid]
  
  # Find approximate matches for suggestions
  suggestions <- lapply(invalid, function(x) {
    matches <- agrep(
      x, valid_vars,
      max.distance = 0.2,
      value = TRUE,
      ignore.case = TRUE
    )
    if (length(matches) == 0) NA_character_ else matches
  })
  
  # Format suggestions
  suggestion_text <- paste(
    mapply(function(var, sug) {
      sug_text <- if (all(is.na(sug))) "No close match" else paste(sug, collapse = "/")
      paste0("{.val ", var, "} → ", sug_text)
    }, invalid, suggestions),
    collapse = ", "
  )
  
  # Abort with structured, informative message
  cli::cli_abort(
    c(
      "!" = "Invalid variable{?s}: {.val {invalid}}.",
      "i" = "Did you mean?: {suggestion_text}"
    ),
    class = "aeme_error_vars_invalid"
  )
}

