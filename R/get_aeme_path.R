#' Get the path to the AEME data
#' @inheritParams build_aeme
#' @param require If TRUE, an error is thrown if the AEME object does not contain
#'  a path to the data. If FALSE, NULL is returned if the AEME object does not 
#'  contain a path to the data.
#' @returns Path to the AEME data
#' @export
get_aeme_path <- function(aeme, require = TRUE) {
  aeme <- check_aeme(aeme)
  cfg <- configuration(aeme)
  path <- cfg[["path"]]
  if (is.null(path)&& require) {
    cli::cli_abort(
      "The AEME object does not contain a path to the data. Please set the path 
      using {.fn build_aeme}."
    )
  }
  return(path)
}
