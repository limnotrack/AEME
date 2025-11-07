#' Get the directory of the lake model setup
#'
#' @inheritParams build_aeme
#'
#' @return character; the directory of the lake model setup
#' @export
#'

get_lake_dir <- function(aeme, path = getwd()) {
  aeme <- check_aeme(aeme = aeme)
  path <- check_path(path = path, must_exist = TRUE)
  lke <- AEME::lake(aeme)
  lakename <- tolower(lke[["name"]])
  lake_dir <- file.path(path, paste0(lke$id, "_", lakename))
  return(lake_dir)
}
