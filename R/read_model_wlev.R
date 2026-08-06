#' Read water level from model output
#' 
#' @description
#' This function reads water level data from the output of supported lake models.
#' 
#' @inheritParams read_model_outputs
#' @importFrom withr local_locale local_timezone
#' @importFrom cli cli_abort
#' @return A data frame with columns:
#' \itemize{
#'  \item `Date`: Date-time of the water level observation (POSIXct, UTC)
#'  \item `LKE_lvlwtr`: Water level (meters)
#'  }
#'  
#' @export

read_model_wlev <- function(nc = NULL, lake_dir, model) {
  
  # Set timezone
  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")
  
  
  model <- check_model(model)
  if (length(model) != 1) {
    cli::cli_abort("Please supply a single model name.")
  }
  if (is.null(nc)) {
    lake_dir <- check_path(lake_dir, must_exist = TRUE)
    # Read in model netCDF file
    nc_files <- get_model_outfile(model = model, path = lake_dir)[[model]]
    if (model == "gotm_wet") {
      nc_file <- nc_files["output"]
    } else {
      nc_file <- nc_files
    }
    nc <- open_nc_safe(file = nc_file, model = model)
    on.exit(ncdf4::nc_close(nc), add = TRUE)
  }

  # ---- 3. dispatch to model-specific extractor
  wlev <- switch(model,
                     "gotm_wet" = read_gotm_wlev(nc),
                     "glm_aed" = read_glm_wlev(nc),
                     "dy_cd" = read_dy_wlev(nc),
                     "simstrat_aed2" = read_simstrat_wlev(nc)
  )
  return(wlev)
}
