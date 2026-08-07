#' Load model hypsograph from configuration
#' @param lake_dir Directory of lake model outputs
#' @param model Model name. One of "gotm_wet", "glm_aed", or "dy_cd".
#' @param file Optional; path directly to the model's hypsograph/
#' configuration file, bypassing the `lake_dir`-based lookup. Defaults to
#' `NULL`.
#' @return Dataframe of hypsograph with columns elev, area, and depth
#' @export
read_model_hypsograph <- function(model, lake_dir, file = NULL) {
  model <- check_model(model)
  if (is.null(file)) {
    lake_dir <- check_path(lake_dir, must_exist = TRUE)
    cfg <- load_model_config(model = model, lake_dir = lake_dir)
  }
  if (model == "gotm_wet") {
    if (is.null(file)) {
      hyps_filename <- cfg$location$hypsograph
      file <- file.path(lake_dir, "gotm_wet", hyps_filename)
    }
    hyps <- read_gotm_hyps(file = file) |> 
      dplyr::mutate(elev = depth)
  } else if (model == "glm_aed") {
    if (!is.null(file)) {
      cfg <- read_nml(nml_file = file)
    }
    lake_btm <- min(cfg$morphometry$H)
    init_depth <- cfg$init_profiles$lake_depth + lake_btm
    hyps <- data.frame(elev = cfg$morphometry$H, area = cfg$morphometry$A) |> 
      dplyr::mutate(depth = elev - init_depth) |> 
      dplyr::arrange(dplyr::desc(elev))
  } else if (model == "dy_cd") {
    if (is.null(file)) {
      file <- get_model_config_files(model = model,
                                     path = lake_dir)[[model]]["stg"]
    }
    stg <- read_dy_stg(file = file)
    hyps <- stg$bathymetry |>
      dplyr::mutate(depth = elev - stg$surface_elev) |>
      dplyr::arrange(dplyr::desc(elev))
  } else if (model == "simstrat_aed2") {
    if (is.null(file)) {
      bathy_filename <- cfg$Input$Morphology
      file <- file.path(lake_dir, "simstrat_aed2", bathy_filename)
    }
    hyps <- read_simstrat_hyps(file = file) |>
      dplyr::mutate(depth = elev - max(elev)) |>
      dplyr::arrange(dplyr::desc(elev))
  }
  return(hyps)
}
