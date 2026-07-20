# Helper: ensure numeric values are within a range
#' @param x Value to check
#' @param name Name of the parameter (for error messages)
#' @param min Minimum acceptable value (inclusive)
#' @param max Maximum acceptable value (inclusive)
#' @return NULL if valid, otherwise an error message
#' @noRd
check_range <- function(x, name, min = -Inf, max = Inf) {
  val <- suppressWarnings(as.numeric(x))
  if (is.na(val)) return(paste(name, "is not numeric"))
  if (val < min || val > max) return(paste(name, "out of range [", min, ", ", max, "]"))
  NULL
}

# Helper: check logical flags
#' @param x Value to check
#' @param name Name of the parameter (for error messages)
#' @return NULL if valid, otherwise an error message
#' @noRd
check_logical <- function(x, name) {
  logical_vals <- c("TRUE", "FALSE", "T", "F", "true", "false")
  if (!is.logical(x) && !(x %in% logical_vals)) {
    return(paste(name, "must be logical (TRUE/FALSE)"))
  }
  NULL
}

# Helper: ensure a file exists if a path is given
#' @param path File path to check
#' @param base_path Base directory to prepend to the path
#' @return NULL if file exists or path is NULL/NA, otherwise an error message
#' @noRd
check_file_if_exists <- function(path, base_path) {
  if (is.null(path) || is.na(path)) return(NULL)
  full <- file.path(base_path, path)
  if (!file.exists(full)) paste("Missing file:", path) else NULL
}

#' Helper: provide a default value if NULL
#' @param x Value to check
#' @param y Default value if x is NULL
#' @return x if not NULL, otherwise y
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

