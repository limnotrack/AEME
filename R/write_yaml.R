#' Write a yaml object to file
#'
#' Write the YAML representation of an R object (list) to a file.
#'  Taken from the `yaml` package but added catch to replace 'yes' with 'true'
#'   and 'no' with 'false' and '~' with 'null' to make it compatible with
#'   running the GOTM model.
#' @inheritParams yaml::write_yaml
#' @export
#' @importFrom yaml write_yaml
#' @author
#' Jeremy Stephens <jeremy.f.stephens@vumc.org>, Tadhg Moore
#' @examples
#'
#' \dontrun{
#' tmpdir <- tempdir()
#' aeme_dir <- system.file("extdata/lake/", package = "AEME")
#' # Copy files from package into tempdir
#' file.copy(aeme_dir, tmpdir, recursive = TRUE)
#' path <- file.path(tmpdir, "lake")
#' yaml <- yaml::read_yaml(file.path(path, "aeme.yaml"))
#'
#' # Set inflows and outflows to NULL
#' yaml$inflows$data <- NULL
#' yaml$outflows$data <- NULL
#'
#' write_yaml(yaml, file.path(path, "aeme_simple.yaml"))
#' }

write_yaml <- function(x, file, fileEncoding = "UTF-8", ...) {
  yaml::write_yaml(x, file, fileEncoding, ...)
  lins <- readLines(file)
  lins <- gsub("\\byes", "true", lins)
  lins <- gsub("\\bno", "false", lins)
  lins <- gsub("~", "", lins)
  writeLines(lins, file)
}
