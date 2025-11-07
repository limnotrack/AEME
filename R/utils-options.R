.onLoad <- function(libname, pkgname) {
  # Set default options when package is loaded
  op <- options()
  op.AEME <- list(
    AEME_inform = TRUE  # default: messages are shown
  )
  # Only set options that are not already defined
  toset <- !(names(op.AEME) %in% names(op))
  if (any(toset)) options(op.AEME[toset])
  
  invisible()
}

#' Inform messages respecting the global AEME_inform option
#'
#' @param ... arguments passed to cli_inform_safe()
#' @noRd
cli_inform_safe <- function(...) {
  if (isTRUE(getOption("AEME_inform", TRUE))) {
    cli::cli_inform(...)
  }
}
