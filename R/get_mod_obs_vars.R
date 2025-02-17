#' Get the variables that are both in the observation and model output
#'
#' @inheritParams get_obs_vars
#' @inheritParams get_output_vars
#'
#' @return A character vector of variables that are in both the observation and
#' model output
#' @export
#'
#' @examples
#' aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
#' aeme <- readRDS(aeme_file)
#' path <- tempdir()
#' model_controls <- get_model_controls(use_bgc = TRUE)
#' model <- c("glm_aed")
#' aeme <- build_aeme(path = path, aeme = aeme, model = model,
#'                    model_controls = model_controls,
#'                    ext_elev = 5, use_bgc = TRUE)
#' # Run models
#' aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
#' path = path, model_controls = model_controls,
#' parallel = TRUE, ncores = 2L)
#' get_mod_obs_vars(aeme = aeme, model = model)
get_mod_obs_vars <- function(aeme, model, ens_n = 1) {
  obs_vars <- get_obs_vars(aeme)
  out_vars <- get_output_vars(aeme, model = model, ens_n = ens_n)
  tgt_vars <- obs_vars[obs_vars %in% out_vars]
  return(tgt_vars)
}
