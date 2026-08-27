#' Populate the GLMv4 `&mass_balance` block from the active AED variables
#'
#' GLMv4 added a `&mass_balance` namelist block that writes a water/mass
#' balance diagnostic CSV for a chosen set of water-quality variables. AEME
#' fills `balance_vars` with the AED variables that are switched on in
#' `model_controls` (translated to their GLM-AED names); when no biogeochemistry
#' is active, or no variable qualifies, it falls back to `balance_varnum = 0`
#' and drops `balance_vars` entirely.
#'
#' Only touches `glm_nml` when it already carries a `&mass_balance` block
#' (i.e. the hydrodynamic template is a glm4.nml); older GLM builds have no
#' such block and are returned unchanged.
#'
#' @param glm_nml list; parsed GLM nml object.
#' @param model_controls data.frame; AEME model controls table.
#' @param use_bgc logical; is the biogeochemistry (AED) library active?
#'
#' @return the updated `glm_nml` list.
#' @noRd
set_glm_mass_balance <- function(glm_nml, model_controls, use_bgc = TRUE) {

  mb <- glm_nml[["mass_balance"]]
  if (is.null(mb)) {
    return(glm_nml)
  }

  wq_names <- character(0)
  if (isTRUE(use_bgc) && !is.null(model_controls)) {
    sim_vars <- model_controls |>
      dplyr::filter(simulate, !is.na(initial_wc),
                    !var_aeme %in% c("HYD_temp", "CHM_salt")) |>
      dplyr::distinct(var_aeme) |>
      dplyr::pull(var_aeme)
    if (length(sim_vars) > 0) {
      wq_names <- rename_modelvars(sim_vars, type_output = "glm_aed",
                                   warn_unmatched = TRUE)
      wq_names <- wq_names[!is.na(wq_names)]
    }
  }

  mb[["balance_file"]] <- mb[["balance_file"]] %||% "mass_balance"
  if (length(wq_names) > 0) {
    mb[["balance_varnum"]] <- length(wq_names)
    mb[["balance_vars"]] <- wq_names
  } else {
    mb[["balance_varnum"]] <- 0
    mb[["balance_vars"]] <- NULL
  }
  mb[["timezone"]] <- mb[["timezone"]] %||% 0

  glm_nml[["mass_balance"]] <- mb
  glm_nml
}
