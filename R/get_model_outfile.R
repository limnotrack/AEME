#' Get model output file
#'
#' @inheritParams build_aeme
#' @param lake_dir Path to the lake AEME directory. If NULL, it will be
#' computed from `aeme` and `path`.
#'
#' @return list of model output files.
#' @export
#'

get_model_outfile <- function(aeme, model, path, lake_dir = NULL) {
  model <- check_model(model = model)
  if (is.null(lake_dir)) {
    aeme <- check_aeme(aeme)
    path <- check_path(path = path, must_exist = TRUE)
    lake_dir <- get_lake_dir(path = path, aeme = aeme)
  }
  cfg_files <- get_model_config_files(model = model, lake_dir = lake_dir)
  out_file <- lapply(model, \(m) {
    if (m == "dy_cd") {
      files <- file.path(lake_dir, m, "DYsim.nc")
    } else if (m == "glm_aed") {
      nml <- read_nml(cfg_files[[m]][["glm3"]])
      files <- file.path(lake_dir, m, nml$output$out_dir, 
                           paste0(nml$output$out_fn, ".nc"))
    } else if (m == "gotm_wet") {
      output <- yaml::read_yaml(cfg_files[[m]][["output"]])
      out_names <- paste0(names(output), ".nc")
      files <- file.path(lake_dir, m, out_names)
    }
    names(files) <- basename(tools::file_path_sans_ext(files))
    return(files)
  })
  names(out_file) <- model

  return(out_file)
}
