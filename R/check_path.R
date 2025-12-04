#' Check and manage file paths
#'
#' @param path Character string specifying the file path to check.
#' @param create Logical indicating whether to create the directory if it does 
#' not exist. Default is FALSE.
#' @param must_exist Logical indicating whether the directory must exist. If 
#' TRUE and the directory does not exist, an error is thrown. Default is FALSE.
#'
#' @returns Normalized file path as a character string.
#' @export
#' 
#' @importFrom cli cli_abort cli_inform
#'
#' @examples
#' check_path("aeme", create = TRUE)
check_path <- function(path, create = FALSE, must_exist = FALSE) {
  # Check that path is provided and valid
  if (missing(path) || is.null(path) || !nzchar(path)) {
    cli::cli_abort("{.arg path} must be a non-empty character string.")
  }
  
  if (length(path) != 1L) {
    cli::cli_abort("{.arg path} must be a single file path, not 
                   {length(path)}.")
  }
  
  # Normalize (resolve relative paths)
  path <- normalizePath(path, mustWork = FALSE)
  
  # Check existence
  if (!dir.exists(path)) {
    if (must_exist) {
      cli::cli_abort(
        c(
          "!" = "Directory does not exist: {.file {path}}.",
          "i" = "Make sure to run {.code build_aeme()} before calling this 
          function."
        ),
        class = "aeme_error_path_missing"
      )
    }
    if (create) {
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
      cli_inform_safe(c("✓" = "Created missing directory {.file {path}}."))
    }
  }
  return(path)
}

