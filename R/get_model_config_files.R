#' Get model configuration files paths
#'
#' @inheritParams build_aeme
#'
#' @returns A named vector with paths to model configuration files
#' @export
#'

get_model_config_files <- function(aeme, model, path) {
  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  if ("glm_aed" %in% model) {
    glm_files <- list.files(
      path = file.path(lake_dir, "glm_aed"),
      pattern = "\\.nml$",
      full.names = TRUE
    )
    names(glm_files) <- basename(tools::file_path_sans_ext(glm_files))
  } else {
    glm_files <- NULL
  }
  if ("gotm_wet" %in% model) {
    gotm_files <- list.files(
      path = file.path(lake_dir, "gotm_wet"),
      pattern = "\\.yaml$",
      full.names = TRUE
    )
    names(gotm_files) <- basename(tools::file_path_sans_ext(gotm_files))
  } else {
    gotm_files <- NULL
  }
  
  if ("dy_cd" %in% model) {
    # Files with .bio, .chm, .sed, .par extensions
    dycd_files <- list.files(
      path = file.path(lake_dir, "dy_cd"),
      pattern = "\\.(bio|chm|sed|par)$",
      full.names = TRUE
    )
    # Name files using their extensions
    names(dycd_files) <- tools::file_ext(basename(dycd_files))
  } else {
    dycd_files <- NULL
  }
  
  all_files <- c(glm_files, gotm_files, dycd_files)
  return(all_files)
}
