#' Match variable to AEME variable names
#' 
#' This function takes a character vector of variable names and checks them 
#' against the official AEME variable names. If an input variable does not match
#' any official name, the function attempts to find the closest match using 
#' both exact keyword matching and fuzzy string matching (Levenshtein distance).
#' The function returns a character vector of the same length as the input, 
#' where each element is either the original variable name (if it was valid) 
#' or the best-matching official AEME variable name. If no suitable match is 
#' found for an invalid variable, a warning is issued.
#'
#' @param x Character vector of variable names to check.
#' @param key_filter Optional string to filter the AEME variable names by a 
#' specific keyword. If provided, only AEME variable names containing this keyword will be
#' considered for matching. This can help improve matching accuracy by limiting the
#' pool of candidate variable names to those relevant to a particular context
#' (e.g., "met" for meteorological variables).
#'
#' @returns Character vector of variable names, with invalid names replaced by 
#' the closest official AEME variable name where possible.
#' @importFrom cli cli_alert_success cli_alert_warning
#' @export
#'
#' @examples
#' guess_aeme_vars(c("temp", "oxy", "ph", "chla", "tp", "tn"))
#' guess_aeme_vars(c("temp", "swr", "lwr", "wind", "precip"), key_filter = "MET")

guess_aeme_vars <- function(x, key_filter) {
  # 1. Load key dataset
  data("key_naming", package = "AEME", envir = environment())
  
  if (!missing(key_filter) && !is.null(key_filter)) {
    key_naming <- key_naming |>
      dplyr::filter(grepl(key_filter, var_aeme, ignore.case = TRUE))  
  }
  
  valid_vars <- key_naming$var_aeme
  keywords_list <- key_naming$keywords
  
  result <- x
  is_valid <- x %in% valid_vars
  to_fix <- which(!is_valid)
  
  if (length(to_fix) == 0) return(x)
  
  for (i in to_fix) {
    current_val <- x[i]
    
    # Use grep for a quick exact-ish match check first
    match_idx <- grep(current_val, keywords_list, ignore.case = TRUE)
    
    # If no grep match, try fuzzy agrep to get a pool of candidates
    if (length(match_idx) == 0) {
      match_idx <- agrep(current_val, keywords_list, 
                         max.distance = 0.2, ignore.case = TRUE)
    }
    
    # 5. Select the best match if multiple candidates exist
    if (length(match_idx) > 0) {
      
      if (length(match_idx) > 1) {
        # Calculate Levenshtein distance between input and the official NAMES
        # This determines which target variable is 'closest' conceptually
        distances <- as.vector(utils::adist(current_val, valid_vars[match_idx], ignore.case = TRUE))
        
        # Pick the index with the minimum distance
        best_match_sub_idx <- which.min(distances)
        final_idx <- match_idx[best_match_sub_idx]
      } else {
        final_idx <- match_idx
      }
      
      result[i] <- valid_vars[final_idx]
      
      cli::cli_alert_success(paste0("Variable '", current_val, 
                                    "' matched to '", result[i], "'."))
    } else {
      cli::cli_alert_warning(paste0("Could not find a match for '", current_val, "'."))
    }
  }
  
  return(result)
}
