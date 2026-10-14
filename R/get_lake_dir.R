#' Get the directory of the lake model setup
#'
#' @inheritParams build_aeme
#'
#' @return character; the directory of the lake model setup
#' @export
#' 
#' @examples
#' aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
#' aeme <- readRDS(aeme_file)
#' path <- tempdir()
#' model <- c("glm_aed")
#' aeme <- build_aeme(aeme = aeme, model = model, path = path, ext_elev = 3)
#' lake_dir <- get_lake_dir(aeme)
#'

get_lake_dir <- function(aeme, path) {
  aeme <- check_aeme(aeme = aeme)
  if (missing(path) || is.null(path)) {
    path <- get_aeme_path(aeme)
  }
  path <- check_path(path = path, must_exist = TRUE)
  lke <- AEME::lake(aeme)
  lakename <- tolower(lke[["name"]])
  lake_dir <- file.path(path, paste0(lke$id, "_", lakename))
  return(lake_dir)
}
