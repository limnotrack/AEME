#' Get the path to the AEME data
#' @inheritParams build_aeme
#' @returns Path to the AEME data
#' @export
get_aeme_path <- function(aeme) {
  aeme <- check_aeme(aeme)
  cfg <- configuration(aeme)
  path <- cfg[["path"]]
  if (is.null(path)) {
    cli::cli_abort(
      "The AEME object does not contain a path to the data. Please set the path using `build_aeme()`."
    )
  }
  return(path)
}
