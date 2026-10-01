#' Check AEME variable names
#'
#' Check if the provided variable names are valid AEME variable names.
#'
#' @param x Character vector of variable names to check.
#' @param aeme Aeme object, optional. If provided, names that are actually
#'   present in `aeme`'s loaded output (see [get_output_vars()]) are also
#'   accepted, even without a `key_naming` entry -- this covers variables
#'   [read_glm_output()] loaded straight from a GLM/AED output file with no
#'   AEME translation (`load_all = TRUE`). Such names are passed through
#'   as-is, without the fuzzy-matching [guess_aeme_vars()] would otherwise
#'   attempt on them.
#'
#' @returns Invisibly returns TRUE if all variables are valid, otherwise throws
#' an error.
#' @importFrom cli cli_abort
#' @export
#'
#' @examples
#' check_aeme_vars("HYD_temp")

check_aeme_vars <- function(x, aeme = NULL) {
  # Load key dataset
  data("key_naming", package = "AEME", envir = environment())
  valid_vars <- key_naming$var_aeme  # column with valid variable names

  loaded_vars <- character()
  if (!is.null(aeme)) {
    loaded_vars <- tryCatch(get_output_vars(aeme), error = function(e) character())
  }
  already_loaded <- x %in% loaded_vars

  # Only fuzzy-match/validate names not already confirmed present in the
  # aeme's own loaded output
  if (any(!already_loaded)) {
    x[!already_loaded] <- guess_aeme_vars(x[!already_loaded])
  }

  # Validate
  is_valid <- (x %in% valid_vars) | already_loaded
  if (all(is_valid)) {
    return(x)
  }
  
  # Identify invalid variables
  invalid <- x[!is_valid]
  
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
      if (all(is.na(sug))) {
        sug_text <- "No close match" 
      } else{
        sug_text <- paste(sug, collapse = "/")
      } 
      paste0(var, " -> ", sug_text)
    }, invalid, suggestions),
    collapse = ", "
  )
  
  cli::cli_abort(
    c(
      "x" = "Invalid variable{?s}: {.val {invalid}}.",
      "i" = paste("Did you mean?:", suggestion_text)
    ),
    class = "aeme_error_vars_invalid"
  )
}

