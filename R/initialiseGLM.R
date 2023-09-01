#' Write initial temperature and salinity profiles to the GLM nml file
#'
#' @inheritParams set_nml
#' @param lvl_bottom numeric; depth of bottom of profile
#' @param init_depth numeric; depth of top of profile
#' @param tmpwtr numeric; water temperature
#' @param tbl_obs data.frame; with profile
#' @param Kw numeric; value of Kw
#'
#' @return GLM nml list object
#' @noRd
#'

initialiseGLM <-  function(glm_nml, lvl_bottom, init_depth,
                           tmpwtr = 10, tbl_obs = NULL, Kw) {

  # define the proTable (intial profiles for T and SAL)
  if (is.null(tbl_obs)) {
    tbl_obs <- data.frame(c(lvl_bottom, init_depth),
                          c(tmpwtr, tmpwtr),
                          c(0, 0))
  }

  arg_list <- list(
    light_mode = 0,
    n_bands = 4,
    light_extc = c(1.0, 0.5, 2.0, 4.0),
    Benthic_Imin = 10,
    Kw = Kw,
    lake_depth = round(init_depth, 2),
    num_depths = nrow(tbl_obs),
    the_depths = round(tbl_obs[, 1], 2),
    the_temps = tbl_obs[, 2],
    the_sals = tbl_obs[, 3]
  )

  glm_nml <- set_nml(glm_nml = glm_nml, arg_list = arg_list)
  return(glm_nml)
}
