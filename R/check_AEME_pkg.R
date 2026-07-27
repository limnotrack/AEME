#' Check if the package is working correctly
#'
#' @returns TRUE if the package is working correctly
#' @export
#' @importFrom cli cli_progress_step cli_inform cli_warn
#' 
check_AEME_pkg <- function() {
  tmpdir <- tempdir()
  aeme_dir <- system.file("extdata/lake/", package = "AEME")
  file.copy(aeme_dir, tmpdir, recursive = TRUE)
  path <- file.path(tmpdir, "lake")
  
  cli::cli_progress_step("Loading AEME object")
  suppressWarnings(suppressMessages(
    aeme <- yaml_to_aeme(path = path, "aeme.yaml")
  ))
  
  lke <- lake(aeme)
  model_controls <- get_model_controls(use_bgc = FALSE)
  os <- .detect_os()
  if (os != "windows") {
    model <- c("glm_aed")
    cli::cli_inform(c("i" = "GLM-AED is the only model available on {.field {os}}"))
  } else {
    model <- c("dy_cd", "glm_aed", "gotm_wet")
  }  
  
  cli::cli_progress_step("Building AEME model ensemble configuration", 
                         msg_done = "AEME model ensemble configuration built")
  suppressWarnings(suppressMessages(
    aeme <- build_aeme(path = path, aeme = aeme, model = model,
                       model_controls = model_controls, ext_elev = 2)
  ))
  
  cli::cli_progress_step("Running AEME model ensemble",
                         msg_done = "AEME model ensemble run complete")
  suppressWarnings(suppressMessages(
    aeme <- run_aeme(aeme = aeme, model = model, verbose = FALSE,
                     model_controls = model_controls, path = path)
  ))
  
  model_outfiles <- get_model_outfile(aeme) |>
    unlist()
  output_present <- any(file.exists(model_outfiles))
  
  cli::cli_progress_step("Checking AEME model output")
  if (output_present) {
    cli::cli_inform(c("v" = "AEME model output present"))
  } else {
    cli::cli_warn(c("!" = "AEME model output not present"))
  }
  
  all(file.exists(model_outfiles))
}
