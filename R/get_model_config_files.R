#' Get model configuration files paths
#'
#' @inheritParams build_aeme
#' @param lake_dir Path to the lake AEME directory. If NULL, it will be
#' computed from `aeme` and `path`.
#' @returns A list with model configuration files paths
#' @export
#'

get_model_config_files <- function(aeme, model, path, lake_dir = NULL) {
  
  model <- check_model(model = model)
  if (is.null(lake_dir)) {
    aeme <- check_aeme(aeme)
    path <- check_path(path = path, must_exist = TRUE)
    lake_dir <- get_lake_dir(aeme = aeme, path = path)
  }
  
  out <- list()
  
  if ("glm_aed" %in% model) {
    glm_files <- list.files(
      path = file.path(lake_dir),
      pattern = "\\.nml$",
      full.names = TRUE,
      recursive = TRUE
    )
    names(glm_files) <- basename(tools::file_path_sans_ext(glm_files))
    out$glm_aed <- glm_files
  }
  
  if ("gotm_wet" %in% model) {
    gotm_files <- list.files(
      path = file.path(lake_dir),
      pattern = "\\.yaml$",
      full.names = TRUE,
      recursive = TRUE
    )
    names(gotm_files) <- basename(tools::file_path_sans_ext(gotm_files))
    out$gotm_wet <- gotm_files
  }
  
  if ("dy_cd" %in% model) {
    dycd_files <- list.files(
      path = file.path(lake_dir),
      pattern = "\\.(bio|chm|sed|par|cfg|con|inf|met|pro|stg|wdr)$",
      full.names = TRUE,
      recursive = TRUE
    )
    names(dycd_files) <- tools::file_ext(basename(dycd_files))
    out$dy_cd <- dycd_files
  }
  
  return(out)
}

