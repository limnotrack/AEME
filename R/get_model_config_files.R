#' Get model configuration files paths
#'
#' @inheritParams build_aeme
#'
#' @returns A list with model configuration files paths
#' @export
#'

get_model_config_files <- function(aeme, model, path) {
  
  aeme <- check_aeme(aeme)
  model <- check_model(model = model)
  path <- check_path(path = path, must_exist = TRUE)
  lake_dir <- get_lake_dir(aeme = aeme, path = path)
  
  out <- list()
  
  if ("glm_aed" %in% model) {
    glm_files <- list.files(
      path = file.path(lake_dir, "glm_aed"),
      pattern = "\\.nml$",
      full.names = TRUE,
      recursive = TRUE
    )
    names(glm_files) <- basename(tools::file_path_sans_ext(glm_files))
    out$glm_aed <- glm_files
  }
  
  if ("gotm_wet" %in% model) {
    gotm_files <- list.files(
      path = file.path(lake_dir, "gotm_wet"),
      pattern = "\\.yaml$",
      full.names = TRUE
    )
    names(gotm_files) <- basename(tools::file_path_sans_ext(gotm_files))
    out$gotm_wet <- gotm_files
  }
  
  if ("dy_cd" %in% model) {
    dycd_files <- list.files(
      path = file.path(lake_dir, "dy_cd"),
      pattern = "\\.(bio|chm|sed|par)$",
      full.names = TRUE
    )
    names(dycd_files) <- tools::file_ext(basename(dycd_files))
    out$dy_cd <- dycd_files
  }
  
  return(out)
}

