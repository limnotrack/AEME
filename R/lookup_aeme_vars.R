#' View AEME variables
#'
#' @param group character string to filter variables by group. Default is NULL.
#' Options include "HYD" (hydrodynamic), "LKE" (Lake), "RAD" (Radiation),
#' "CHM" (Chemistry), "PHS" (Phosphorus), "NIT" (Nitrogen), "CAR" (Carbon),
#'  "SIL" (Silica), "MET" (Meteorologica), "ZOO" (Zooplankton),
#'  "PHY" (Phytoplankton),
#' "MAC" (Macrophytes), "NCS" (Non-conservative substances).
#' @param var_aeme character vector of exact AEME variable names to filter by
#' (e.g., "HYD_temp", "CHM_oxy", "PHS_tp"). Default is NULL.
#' @param name character string to filter variables by partial match in either
#' the variable name or descriptive text. Default is NULL.
#'
#' @importFrom utils data
#' @importFrom dplyr select rename filter
#'
#' @returns A data frame with AEME variables
#' @export
#'
#' @examples
#' # Filter by group keyword in var_aeme
#' lookup_aeme_vars(group = "NIT")
#' 
#' # Filter by exact variable name
#' lookup_aeme_vars(var_aeme = "NIT_tn")
#' 
#' # Filter by partial name or text match
#' lookup_aeme_vars(name = "chlorophyll")
#' 
#' # Combine filters
#' lookup_aeme_vars(group = "NIT", var_aeme = "HYD_temp", name = "phosphate")

lookup_aeme_vars <- function(group = NULL, var_aeme = NULL, name = NULL) {
  # Load dataset
  utils::data("key_naming", package = "AEME", envir = environment())
  
  df <- key_naming |>
    dplyr::select(name, name_text, units) |>
    dplyr::rename(var_aeme = name, text = name_text, units = units)
  
  # If all filters are NULL, return all
  if (is.null(group) && is.null(var_aeme) && is.null(name)) {
    return(df)
  }
  
  # Create logical masks
  group_mask <- if (!is.null(group)) {
    grepl(paste(group, collapse = "|"), df$var_aeme, ignore.case = TRUE)
  } else {
    rep(FALSE, nrow(df))
  }
  
  var_mask <- if (!is.null(var_aeme)) {
    df$var_aeme %in% var_aeme
  } else {
    rep(FALSE, nrow(df))
  }
  
  name_mask <- if (!is.null(name)) {
    grepl(paste(name, collapse = "|"), df$var_aeme, ignore.case = TRUE) |
      grepl(paste(name, collapse = "|"), df$text, ignore.case = TRUE)
  } else {
    rep(FALSE, nrow(df))
  }
  
  # OR logic across all masks
  keep <- group_mask | var_mask | name_mask
  
  df <- df |> 
    dplyr::filter(keep) |>
    dplyr::arrange(var_aeme)
  
  return(df)
}
