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
    if ("aed" %in% names(glm_files)) {
      aed_nml <- read_nml(glm_files["aed"])
      # Recursively search for "dbase" in the list aed_nml
      csv_file_sections <- c("aed_phytoplankton", "aed_zooplankton",
                             "aed_macrophyte")
      # Extract dbase value from each section
      for (section in csv_file_sections) {
        if (section %in% names(aed_nml)) {
          dbase_value <- aed_nml[[section]]$dbase
          if (!is.null(dbase_value)) {
            csv_file_path <- file.path(dirname(glm_files["glm3"]), dbase_value)
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

