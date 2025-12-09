#' Get number of sediment zones in GLM-AED model
#'
#' @inheritParams build_aeme
#'
#' @returns Number of sediment zones
#' @export
#'

get_glm_sed_zones <- function(aeme, path, lake_dir = NULL) {
  if (is.null(lake_dir)) {
    if (missing(aeme) | missing(path)) {
      cli::cli_abort("Either {.arg lake_dir} or both {.arg aeme} and
                     {.arg path} must be provided.")
    }
    lake_dir <- get_lake_dir(aeme = aeme, path = path)
  }
  model_config <- read_model_config(model = "glm_aed", path = path)
  n_zones <- model_config$hydrodynamic$sediment$n_zones
  return(n_zones)
}
