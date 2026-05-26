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

initialise_glm <-  function(glm_nml, lvl_bottom, init_depth,
                           tmpwtr = 10, tbl_obs = NULL, Kw, model_controls) {

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
  
  # Add initial AED values
  sim_vars <- model_controls |> 
    dplyr::filter(simulate, !is.na(initial_wc), 
                  !var_aeme %in% c("HYD_temp", "CHM_salt"))
  if (length(sim_vars) > 0) {
    depths <- tbl_obs[["depth"]]
    glm_wq_vars <- sim_vars |> 
      dplyr::mutate(value = initial_wc * conversion_aed) |>
      dplyr::group_by(var_aeme) |>
      # Duplicate each row by number of depths
      dplyr::slice(rep(1:n(), each = length(depths))) 
    var_names <- glm_wq_vars |> 
      dplyr::distinct(var_aeme) |>
      dplyr::pull(var_aeme)
    if (length(var_names) > 0) {
      wq_names <- rename_modelvars(var_names, type_output = "glm_aed")
      num_wq_vars <- length(var_names)
      wq_init_vals <- glm_wq_vars[["value"]]
    } else {
      wq_names <- "''"
      num_wq_vars <- 0
      wq_init_vals <- 0
    }
    arg_list[["wq_names"]] <- wq_names
    arg_list[["num_wq_vars"]] <- num_wq_vars
    arg_list[["wq_init_vals"]] <- wq_init_vals
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
