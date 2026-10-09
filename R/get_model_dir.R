#' Get the model directory for a given AEME and model.
#'
#' @inheritParams build_aeme
#'
#' @returns Path to the model directory
#' @export
#'
#' @examples
#' aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
#' aeme <- readRDS(aeme_file)
#' path <- tempdir()
#' model <- c("glm_aed")
#' aeme <- build_aeme(aeme = aeme, model = model, path = path, ext_elev = 3)
#' lake_dir <- get_lake_dir(aeme)
#' model_dir <- get_model_dir(aeme)
#' 
get_model_dir <- function(aeme, model, path) {
  aeme <- check_aeme(aeme)
  if (missing(model)) {
    model <- list_models(aeme)
  }
  model <- check_model(model = model, os_valid = TRUE)
  aeme <- set_model(aeme = aeme, model = model)
  if (missing(path)) {
    path <- get_aeme_path(aeme)
  }
  path <- check_path(path = path, must_exist = TRUE)
  model_dir <- file.path(path, model)
  names(model_dir) <- model
  return(model_dir)
}
