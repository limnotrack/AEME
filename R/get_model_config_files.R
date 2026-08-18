#' Get model configuration files paths
#'
#' @inheritParams build_aeme
#' @param lake_dir `r lifecycle::badge("deprecated")` Use `path` instead of
#'  `lake_dir`
#' @returns A list with model configuration files paths
#' @export
#'

get_model_config_files <- function(aeme = NULL, model, path = NULL, lake_dir) {

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

  out <- list()
  
  if ("glm_aed" %in% model) {
    glm_files <- .find_model_files(lake_dir, "glm_aed", "\\.nml$")
    names(glm_files) <- basename(tools::file_path_sans_ext(glm_files))
    if ("aed" %in% names(glm_files)) {
      aed_nml <- read_nml(glm_files["aed"])
      # Recursively search for "dbase" in the list aed_nml
      csv_file_sections <- c("aed_phytoplankton", "aed_zooplankton",
                             "aed_macrophyte")
      # dbase paths are relative to the GLM hydrodynamic nml's own directory
      # (e.g. glm3.nml/glm4.nml), not the aed/ subdirectory
      glm_key <- find_glm_nml_key(names(glm_files), must_exist = FALSE)
      glm_dir <- if (!is.na(glm_key)) dirname(glm_files[glm_key]) else lake_dir
      # Extract dbase value from each section
      for (section in csv_file_sections) {
        if (section %in% names(aed_nml)) {
          dbase_value <- aed_nml[[section]]$dbase
          if (!is.null(dbase_value)) {
            csv_file_path <- file.path(glm_dir, dbase_value)
            if (file.exists(csv_file_path)) {
              csv_name <- basename(tools::file_path_sans_ext(dbase_value))
              glm_files[csv_name] <- csv_file_path
            }
          }
        }
      }

    }
    out$glm_aed <- glm_files
  }
  
  if ("gotm_wet" %in% model) {
    gotm_files <- .find_model_files(lake_dir, "gotm_wet", "\\.yaml$")
    names(gotm_files) <- basename(tools::file_path_sans_ext(gotm_files))
    out$gotm_wet <- gotm_files
  }

  if ("dy_cd" %in% model) {
    dycd_files <- .find_model_files(
      lake_dir, "dy_cd", "\\.(bio|chm|sed|par|cfg|con|inf|met|pro|stg|wdr)$"
    )
    names(dycd_files) <- tools::file_ext(basename(dycd_files))
    out$dy_cd <- dycd_files
  }

  if ("simstrat_aed2" %in% model) {
    simstrat_files <- .find_model_files(lake_dir, "simstrat_aed2", "\\.(par|nml)$")
    names(simstrat_files) <- basename(tools::file_path_sans_ext(simstrat_files))
    out$simstrat_aed2 <- simstrat_files
  }

  return(out)
}

#' Recursively find files for one model, scoped to avoid cross-model clashes
#'
#' Prefers a subdirectory of `dir` named after `model` (e.g. `dir/glm_aed`)
#' if one exists -- this is what keeps a search from picking up another
#' model's file when several share an extension (e.g. `dy_cd` and
#' `simstrat_aed2` both use `.par`) inside a shared ensemble `lake_dir`.
#' Falls back to searching `dir` itself recursively when no such
#' subdirectory exists, so `dir` can also be the model's own directory
#' passed directly (not just an ensemble root).
#'
#' @param dir directory to search.
#' @param model character; single model code.
#' @param pattern regex passed to `list.files(pattern = )`.
#' @return character vector of matching file paths.
#' @noRd
.find_model_files <- function(dir, model, pattern) {
  scoped_dir <- file.path(dir, model)
  search_dir <- if (dir.exists(scoped_dir)) scoped_dir else dir
  list.files(
    path = search_dir,
    pattern = pattern,
    full.names = TRUE,
    recursive = TRUE
  )
}
