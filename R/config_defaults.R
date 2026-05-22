#' @title config_defaults
#' @description Internal function to set default configuration values for the 
#' AEME model setup. This function is not intended to be called directly by 
#' users, but rather serves as a helper to initialize configuration settings 
#' with sensible defaults.
#' @noRd
config_defaults <- function() {
  cfg_dflt <- list(
    model_controls = data.frame(),
    use_bgc        = FALSE,
    path           = getwd(),
    ext_elev       = 0,
    calc_wbal      = TRUE,
    wb_method      = 2L,
    calc_wlev      = TRUE,
    hum_type       = 3L,
    est_swr_hr     = TRUE,
    dy_cd          = list(hydrodynamic = list(), bgc = list()),
    glm_aed        = list(hydrodynamic = list(), bgc = list()),
    gotm_wet       = list(hydrodynamic = list(), bgc = list())
  )
}
