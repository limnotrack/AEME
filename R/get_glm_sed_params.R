#' Get number of sediment zones in GLM-AED model
#'
#' @inheritParams build_aeme
#' @inheritParams get_model_outfile
#'
#' @returns Number of sediment zones
#' @export
#'

get_glm_sed_params <- function(aeme, path, lake_dir = NULL) {
  if (is.null(lake_dir)) {
    if (missing(aeme)) {
      cli::cli_abort("Either {.arg lake_dir} or {.arg aeme} must be provided.")
    }
    if (missing(path)) {
      path <- get_aeme_path(aeme)
    }
    lake_dir <- get_lake_dir(aeme = aeme, path = path)
  }
  model_config <- read_model_config(model = "glm_aed", lake_dir = lake_dir)
  params <- lapply(names(model_config$hydrodynamic$sediment), \(par) {
    value <- model_config$hydrodynamic$sediment[[par]]
    if (length(value) > 1) {
      index <- seq_along(value)
    } else {
      index <- NA_integer_
    }
    data.frame(
      model = "glm_aed",
      file = "glm3.nml",
      name = paste0("sediment/", par),
      value = value,
      min = value,
      max = value,
      index = index,
      group = NA_character_
    )
  }) |> 
    dplyr::bind_rows()
  return(params)
}
