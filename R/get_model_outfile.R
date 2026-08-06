#' Get model output file
#'
#' @inheritParams build_aeme
#' @param path Directory to search for the model output. If `aeme` is also
#' provided, `path` is the root combined with `aeme` to compute the lake's
#' directory (as in `get_lake_dir()`) -- omit it to use `aeme`'s own stored
#' path. If `aeme` is not provided, `path` is searched directly, and can be
#' either an ensemble root or a single model's own directory.
#' @param lake_dir `r lifecycle::badge("deprecated")` Use `path` instead of
#'  `lake_dir`
#' 
#' @importFrom cli cli_abort
#'
#' @return list of model output files.
#' @export
#'

get_model_outfile <- function(aeme = NULL, model, path = NULL, lake_dir) {

  
  # Soft deprecate lake_dir arg
  if (!missing(lake_dir)) {
    lifecycle::deprecate_warn(
      when = "0.4.0",
      what = "get_model_config_files(lake_dir)",
      details = "Use `path` instead of `lake_dir`"
    )
    path <- lake_dir
  }
  
  if (is.null(aeme) && is.null(path)) {
    cli::cli_abort("Either `aeme` or `path` must be provided")
  }

  if (is.null(aeme)) {
    lake_dir <- check_path(path = path, must_exist = TRUE)
  } else {
    aeme <- check_aeme(aeme)
    lake_dir <- get_lake_dir(aeme = aeme, path = path)
  }

  if (missing(model)) {
    model <- list_models(aeme)
  } else {
    model <- check_model(model = model)
  }

  # Get config files once
  cfg_files <- get_model_config_files(model = model, lake_dir = lake_dir)
  
  # Map of model-specific resolvers
  resolvers <- list(
    dy_cd = resolve_dy_cd,
    glm_aed = resolve_glm_aed,
    gotm_wet = resolve_gotm_wet,
    simstrat_aed2 = resolve_simstrat_aed2
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
  model_dir <- dirname(cfg[["glm3"]])
  
  # Search recursively
  files <- list.files(
    path = model_dir,
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
  if (length(files) == 0) {
    files <- file.path(lake_dir, "dy_cd", "DYsim.nc")
  }
  names(files) <- "DYsim"
  files
}

#' Model-specific resolvers
#' @noRd
resolve_simstrat_aed2 <- function(lake_dir, cfg) {
  model_dir <- dirname(cfg[["simstrat"]])
  files <- list.files(
    path = model_dir,
    pattern = "^output\\.nc$",
    full.names = TRUE,
    recursive = TRUE
  )
  names(files) <- tools::file_path_sans_ext(basename(files))
  files
}

#' Model-specific resolvers
#' @noRd
resolve_gotm_wet <- function(lake_dir, cfg) {
  output <- yaml::read_yaml(cfg[["output"]])
  out_names <- paste0(basename(names(output)), "\\.nc$")
  model_dir <- dirname(cfg[["output"]])
  
  files <- unlist(lapply(out_names, function(pat) {
    list.files(
      path = model_dir,
      pattern = pat,
      full.names = TRUE,
      recursive = TRUE
    )
  }))
  
  names(files) <- tools::file_path_sans_ext(basename(files))
  files
}

