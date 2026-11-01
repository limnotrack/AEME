#' View the AED module interaction diagram
#'
#' Opens an interactive HTML diagram of the interactions between the modules
#' of the Aquatic EcoDynamics (AED) library used by GLM-AED and Simstrat-AED2.
#' The diagram is shown in the RStudio/Positron viewer pane when available,
#' otherwise in the default web browser.
#'
#' @param browser Logical; if `TRUE`, always open in the web browser instead of
#'   the viewer pane. Default is `FALSE`.
#' @param output Character; optional path to copy the HTML file to. If `NULL`
#'   (default), a temporary file is used.
#'
#' @return Invisibly returns the path to the HTML file.
#' @export
#'
#' @examples
#' \dontrun{
#' view_aed_modules()
#' view_aed_modules(browser = TRUE)
#' }
view_aed_modules <- function(browser = FALSE, output = NULL) {
  src <- system.file("extdata", "aed_modules", "aed_module_interactions.html",
                     package = "AEME")
  if (!nzchar(src)) {
    cli::cli_abort("Cannot find the AED module diagram in the AEME installation.")
  }
  if (is.null(output)) {
    # The RStudio viewer only displays files located in the session temp dir
    output <- file.path(tempfile("aed_modules"), "aed_module_interactions.html")
  }
  dir.create(dirname(output), showWarnings = FALSE, recursive = TRUE)
  file.copy(src, output, overwrite = TRUE)

  if (interactive()) {
    viewer <- getOption("viewer")
    if (!browser && is.function(viewer)) {
      viewer(output)
    } else {
      utils::browseURL(output)
    }
  }
  invisible(output)
}
