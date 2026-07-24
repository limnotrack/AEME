#' Read in model configuration files for a given model and lake directory
#'
#' @param model character; model name ("dy_cd", "glm_aed", "gotm_wet"). Only
#'  one model at a time.
#' @param path character; directory which contains the model configuration files.
#'
#' @returns List with model configuration components. This includes a 'hydrodynamic'
#' list with hydrodynamic model configuration and a 'bgc' list with biogeochemistry
#' model configuration (if applicable).
#' @export
#'

read_model_config <- function(model, lake_dir) {
  model <- check_model(model)
  path <- check_path(lake_dir, must_exist = TRUE)
  out <- list()
  if (length(model) != 1) {
    cli::cli_abort("Please provide only one model at a time. {.arg model} has 
                   {length(model)} models {.val {model}}.")
  }
  model_cfg_files <- get_model_config_files(lake_dir = lake_dir, 
                                            model = model)[[model]]
  cfg <- lapply(model_cfg_files, \(f) {
    file_type <- tools::file_ext(f)
    if (file_type == "nml") {
      read_nml(f)
    } else if (file_type %in% c("csv", "tsv")) {
      read_aed_param_csv(f)
    } else if (file_type == "yaml") {
      yaml::read_yaml(file = f)
    } else if (file_type == "par" && model == "simstrat_aed2") {
      # Only Simstrat's own .par file is JSON - dy_cd's dyresm3p1.par shares
      # the same extension but is plain text, and falls through to
      # readLines() below like the rest of DYRESM-CAEDYM's config files.
      jsonlite::fromJSON(f, simplifyVector = FALSE)
    } else {
      readLines(f)
    }
  })

  if (model == "dy_cd") {
    out$hydrodynamic <- list(par = cfg$par, cfg = cfg$cfg)
    # Remove par and cfg from list
    cfg$par <- NULL
    cfg$cfg <- NULL
  } else if (model == "glm_aed") {
    out$hydrodynamic <- cfg[["glm3"]]
    # Remove glm3 from list
    cfg[["glm3"]] <- NULL
  } else if (model == "gotm_wet") {
    out$hydrodynamic <- list(gotm = cfg[["gotm"]],
                         output = cfg[["output"]])
    # Remove gotm and output from list
    cfg[["gotm"]] <- NULL
    cfg[["output"]] <- NULL
  } else if (model == "simstrat_aed2") {
    out$hydrodynamic <- cfg[["simstrat"]]
    # Remove simstrat from list
    cfg[["simstrat"]] <- NULL
  }
  if (length(cfg) > 0) {
    out$bgc <- cfg
  }
  return(out)
}
