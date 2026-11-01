#' Component variables of the GLM-AED `aed_totals`
#'
#' Uses the same logic as `derive_aed_totals()` (internal-quota pools versus
#' biomass x fixed stoichiometry for each phytoplankton group) to list every
#' variable summed into TN, TP, TOC (and TSS), so they can be reported in the
#' GLMv4 `&mass_balance` block.
#'
#' @param path_glm directory holding the GLM nml and `aed/` configuration.
#' @return named list (`TN`, `TP`, `TOC`, `TSS` where present) of GLM variable
#'   names, or `NULL` if the totals cannot be derived (e.g. no phytoplankton
#'   parameters present).
#' @noRd
glm_totals_component_vars <- function(path_glm) {
  if (is.null(path_glm)) return(NULL)
  res <- tryCatch({
    cfg <- read_model_config(model = "glm_aed", lake_dir = dirname(path_glm))
    list(totals = derive_aed_totals(cfg)[["bgc"]][["aed"]][["aed_totals"]],
         models = glm_active_aed_models(cfg[["bgc"]][["aed"]]))
  }, error = function(e) NULL)
  if (is.null(res)) return(NULL)
  totals <- res$totals
  nms <- sub("_vars$", "", grep("_vars$", names(totals), value = TRUE))
  stats::setNames(lapply(nms, function(g) {
    vars <- totals[[paste0(g, "_vars")]]
    # Drop components whose AED module is not active
    unique(vars[glm_aed_var_active(vars, res$models)])
  }), nms)
}
