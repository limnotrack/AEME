#' Check AEME variable names
#' 
#' Check if the provided variable names are valid AEME variable names.
#'
#' @param vars Character vector of variable names to check.
#'
#' @returns Invisibly returns TRUE if all variables are valid, otherwise throws 
#' an error.
#' @export
#'
#' @examples
#' check_aeme_vars("HYD_temp")

check_aeme_vars <- function(vars) {
  # Load dataset
  utils::data("key_naming", package = "AEME", envir = environment())
  
  valid_vars <- key_naming$name  # column with valid var names
  
  # Which are valid
  is_valid <- vars %in% valid_vars
  
  if (all(is_valid)) {
    # All good: return valid names
    return(vars)
  } else {
    # Identify invalid variables
    invalid <- vars[!is_valid]
    
    # Find approximate matches for suggestions
    suggestions <- lapply(invalid, function(x) {
      matches <- agrep(x, valid_vars, max.distance = 0.2, value = TRUE, 
                       ignore.case = TRUE)
      if (length(matches) == 0) {
        NA_character_
      } else {
        matches
      }
    })
    
    # Format message
    msg <- paste0(
      "Invalid variable(s): ", paste(invalid, collapse = ", "), "\n",
      "Did you mean?: ", paste(sapply(suggestions, function(x) paste(x, collapse = "/")), collapse = ", ")
    )
    
    stop(msg)
  }
}
