#' Generate variable mapping code for lake observation data
#' 
#' This function generates R code to create a variable mapping data frame (`var_map`)
#' used in the \code{lake_obs_to_aeme()} function. It takes a data frame of 
#' lake observation data and fuzzy matches variable names to their corresponding
#' AEME variable names using the `key_naming` dataset.
#' When inputting the units of your data, please ensure they are in the format
#' recognized by the `units` package (e.g. "m", "m^3", "degC", "mg/L", "g/m^3"). 
#'
#' @inheritParams lake_obs_to_aeme
#' 
#' @importFrom tibble tibble
#'
#' @returns A character string containing R code to create the `var_map` data 
#' frame. The code can be copied and pasted into your script and modified as 
#' needed.
#' @export
#'
#' @examples
#' #' # Example data frame with lake observation variable names
#' obs_data <- data.frame(
#'  name = c("Water Temp (°C)", "pH", "DO (mg/L)", "Chlorophyll-a (µg/L)"),
#'  value = c(20.5, 7.8, 8.5, 15.2)
#' )
#' # Generate variable mapping code
#' generate_var_map_code(obs_data, var_col_name = "name")

generate_var_map_code <- function(data, var_col_name = "name") {
  
  data("key_naming", package = "AEME", envir = environment())
  
  # Check is data a data frame
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }
  if (!var_col_name %in% names(data)) {
    stop(paste("Column", var_col_name, "not found in data frame."))
  }
  
  # Extract unique variable names
  var_names <- unique(data[[var_col_name]])
  
  # Match with key_naming$name_text and extract var_aeme
  aeme_vars <- guess_var_aeme(var_names)
  
  # Create skeleton data frame
  skeleton <- tibble::tibble(
    var_aeme = aeme_vars,
    name = var_names,
    unit = key_naming$units[match(aeme_vars, key_naming$name)]
  ) |> 
    dplyr::arrange(var_aeme)
  
  # Convert to tribble code
  tribble_lines <- apply(skeleton, 1, function(row) {
    sprintf('  "%s", "%s", "%s",', row[["var_aeme"]], row[["name"]], row[["unit"]])
  })
  
  code <- paste0(
    "var_map <- tibble::tribble(\n",
    "  ~var_aeme, ~name, ~unit,\n",
    paste(tribble_lines, collapse = "\n"),
    "\n)"
  )
  
  message("Copy and paste the following code to create your variable mapping:\n")
  cat(code)
  message("\nUpdate the units to match the units in your data where necessary.")
  invisible(code)
}


#' Guess AEME variable names from input names using fuzzy matching
#' @param var_names Vector of input variable names
#' @param maxDist Maximum string distance to consider a match
#' @importFrom dplyr filter
#' @return Vector of guessed AEME variable names (or NA if no match)
#' @noRd
guess_var_aeme <- function(var_names, maxDist = 8) {
  
  data("key_naming", package = "AEME", envir = environment())
  key_naming_sub <- key_naming |> 
    dplyr::filter(grepl("HYD|LKE_lvlwtr|RAD_secchi|CHM|PHS|NIT|CAR|PHY", name))
  
  var_names_clean <- sanitise_string(var_names)

  v = var_names_clean[1]
  aeme_vars <- sapply(var_names_clean, function(v) {
    idx <- closest_keyword_index(v, key_naming_sub$keywords, maxDist = maxDist)
    if (is.na(idx)) return(NA)
    return(key_naming_sub$name[idx])
    
  })
  names(aeme_vars) <- var_names
  return(aeme_vars)
}

#' Helper to sanitise strings for matching
#' @param x Input string
#' @return Sanitised string
#' @noRd
sanitise_string <- function(x) {
  x |>
    tolower() |>
    gsub("\\(.*?\\)", "", x = _) |>      # remove (m), (°C), etc.
    gsub("[^a-z0-9]+", "_", x = _) |>    # punctuation → _
    gsub("^_|_$", "", x = _) |>              # trim _ from ends
    trimws() 
}

#' Find index of closest matching keyword entry
#' @param input Input string
#' @param keywords Vector of keyword entries (can have synonyms separated by |)
#' @param maxDist Maximum string distance to consider a match
#' @importFrom stringdist stringdist
#' @return Index of closest matching keyword entry, or NA if no match within 
#' maxDist
#' @noRd
closest_keyword_index <- function(input, keywords, maxDist = 10) {
  input_clean <- sanitise_string(input)
  
  # Compute distance per keyword entry
  distances <- sapply(keywords, function(entry) {
    syns <- strsplit(entry, "\\|")[[1]]
    syns_clean <- sanitise_string(syns)
    min(stringdist::stringdist(input_clean, syns_clean))  # best match within this entry
  })
  
  best_idx <- which.min(distances)
  if (distances[best_idx] <= maxDist) {
    return(best_idx)
  } else {
    return(NA_integer_)  # no close enough match
  }
}
