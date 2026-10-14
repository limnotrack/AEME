#' Write initial temperature and salinity profiles to the GLM nml file
#'
#' @inheritParams set_nml
#' @param lvl_bottom numeric; depth of bottom of profile
#' @param init_depth numeric; depth of top of profile
#' @param tmpwtr numeric; water temperature
#' @param tbl_obs data.frame; with profile
#' @param Kw numeric; value of Kw
#' @param aed_models character vector of active AED modules; initial
#'   conditions for variables of inactive modules are omitted. `NULL` (default)
#'   applies no filter.
#'
#' @return GLM nml list object
#' @noRd
#'

initialise_glm <-  function(glm_nml, lvl_bottom, init_depth,
                           tmpwtr = 10, tbl_obs = NULL, Kw, model_controls,
                           aed_models = NULL) {

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
  
  # Add initial AED values. Drop the variables that are not GLM-AED
  # water-column state variables (totals, particulate-inorganic pools,
  # PHY_tchla, NCS_ss*, forcing columns) -- GLM aborts with
  # "Cannot find <var> for initial value" if they reach wq_names.
  # Variables whose AED module is not active (`aed_models`) are dropped too --
  # e.g. CAR_pH without aed_carbon, ZOO_zoo1 without aed_zooplankton.
  wq <- glm_wq_init_args(model_controls, n_depths = arg_list[["num_depths"]],
                         aed_models = aed_models)
  if (!is.null(wq)) {
    arg_list[["wq_names"]] <- wq[["wq_names"]]
    arg_list[["num_wq_vars"]] <- wq[["num_wq_vars"]]
    arg_list[["wq_init_vals"]] <- wq[["wq_init_vals"]]
  }
  
  init_args_req <- c("wq_names", "num_wq_vars", "wq_init_vals")
  for (arg in init_args_req) {
    if (!arg %in% glm_nml[["init_profiles"]]) {
      val <- ifelse(arg == "wq_names", "''", 0)
      glm_nml[["init_profiles"]][[arg]] <- val
    }
  }

  glm_nml <- set_nml(glm_nml = glm_nml, arg_list = arg_list)
  return(glm_nml)
}
