#' Get model output file
#'
#' @inheritParams build_aeme
#'
#' @return list of model output files.
#' @export
#'

get_model_outfile <- function(aeme, model, path) {
  lake_dir <- get_lake_dir(path = path, aeme = aeme)
  cfg <- configuration(aeme)
  out_file <- lapply(model, \(m) {
    if (m == "dy_cd") {
      file.path(lake_dir, m, "DYsim.nc")
    } else if (m == "glm_aed") {
      file.path(lake_dir, m, cfg[[m]]$hydrodynamic$output$out_dir,
                paste0(cfg[[m]]$hydrodynamic$output$out_fn, ".nc"))
    } else if (m == "gotm_wet") {
      out_names <- paste0(names(cfg[[m]]$hydrodynamic$output), ".nc")
      file.path(lake_dir, m, out_names)
    }
  })
  names(out_file) <- model

  return(out_file)
}
