.onLoad <- function(libname, pkgname) {
  # Set default options when package is loaded
  op <- options()
  op.AEME <- list(
    AEME.inform = TRUE,  # default: messages are shown
    AEME.glm_exec = NULL
  )
  # Only set options that are not already defined
  toset <- !(names(op.AEME) %in% names(op))
  if (any(toset)) options(op.AEME[toset])
  
  invisible()
}

#' Inform messages respecting the global AEME.inform option
#'
#' @param ... arguments passed to cli_inform_safe()
#' @noRd
cli_inform_safe <- function(...) {
  if (isTRUE(getOption("AEME.inform", TRUE))) {
    cli::cli_inform(...)
  }
}

#' Inform messages respecting the global AEME.inform option
#' @param ... arguments passed to cli_inform_safe()
#' @param FUN function to use for messaging, default is cli::cli_inform
#' @param indent logical, whether to indent the message, default is FALSE
#' @inheritParams cli::cli_abort
#' @export
cli_safe <- function(..., FUN = cli::cli_bullets, indent = TRUE,
                     .envir = parent.frame()) {
  if (isTRUE(getOption("AEME.inform", TRUE))) {
    if (indent) {
      d <- cli::cli_div(theme = list(".bullet" = list("margin-left" = 2)))
      on.exit(cli::cli_end(d))
    }
    FUN(..., .envir = .envir)
  }
}

#' Inform messages respecting the global AEME.inform option
#'
#' @param ... arguments passed to cli_inform_safe()
#' @noRd
cli_table_safe <- function(...) {
  if (isTRUE(getOption("AEME.inform", TRUE))) {
    cat(..., sep = "\n")
  }
}
