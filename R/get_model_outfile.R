#' Get model output file
#'
#' @inheritParams build_aeme
#' @param lake_dir Path to the lake AEME directory. If NULL, it will be
#' computed from `aeme` and `path`.
#'
#' @return list of model output files.
#' @export
#'

get_model_outfile <- function(aeme = NULL, model, path = NULL, lake_dir = NULL) {
  model <- check_model(model)
  
  # --- Resolve lake_dir as needed ---
  if (is.null(lake_dir)) {
    aeme <- check_aeme(aeme)
    path <- check_path(path = path, must_exist = TRUE)
    lake_dir <- get_lake_dir(path = path, aeme = aeme)
  }
  
  # Get config files once
  cfg_files <- get_model_config_files(model = model, lake_dir = lake_dir)
  
  # Map of model-specific resolvers
  resolvers <- list(
    dy_cd = resolve_dy_cd,
    glm_aed = resolve_glm_aed,
    gotm_wet = resolve_gotm_wet
  )
  
  # Loop over models and resolve paths
  out_files <- lapply(model, function(m) {
    resolvers[[m]](lake_dir = lake_dir, cfg = cfg_files[[m]])
  })
  names(out_files) <- model
  
  return(out_files)
}

#' Model-specific resolvers
#' @noRd
resolve_glm_aed <- function(lake_dir, cfg) {
  nml <- read_nml(cfg[["glm3"]])
  
  # Expected basename
  expected_name <- paste0(nml$output$out_fn, ".nc")
  
  # Search recursively
  files <- list.files(
    path = lake_dir,
    pattern = paste0("^", expected_name, "$"),
    full.names = TRUE,
    recursive = TRUE
  )
  
  names(files) <- tools::file_path_sans_ext(basename(files))
  files
}

#' Model-specific resolvers
#' @noRd
resolve_dy_cd <- function(lake_dir, cfg) {
  files <- list.files(
    path = lake_dir,
    pattern = "^DYsim\\.nc$",
    full.names = TRUE,
    recursive = TRUE
  )
  names(files) <- "DYsim"
  files
}

#' Model-specific resolvers
#' @noRd
resolve_gotm_wet <- function(lake_dir, cfg) {
  output <- yaml::read_yaml(cfg[["output"]])
  out_names <- paste0(names(output), "\\.nc$")
  
  files <- unlist(lapply(out_names, function(pat) {
    list.files(
      path = lake_dir,
      pattern = pat,
      full.names = TRUE,
      recursive = TRUE
    )
  }))
  
  names(files) <- tools::file_path_sans_ext(basename(files))
  files
}

