#' Map model(s) to their water-balance evaporation family
#'
#' `dy_cd` and `glm_aed` use the exact same bulk aerodynamic evaporation
#' formula in `simulate_lake_nudged()`, so they share one fitted water
#' balance outflow parameter set (C, h_inv); `gotm_wet` and `simstrat_aed2`
#' each use their own distinct evaporation formula and so need their own
#' fit. The family key is the "representative" model for that formula, so
#' the shared `dy_cd`/`glm_aed` family is keyed by `"glm_aed"`.
#'
#' This is the single source of truth for that grouping, used by
#' `calc_water_balance()`, `get_wbal_param()`, `set_wbal_param()`, and
#' `reset_wbal_param()` so they all agree on which models share a fit.
#'
#' @param model character vector of model name(s), e.g. from `list_models()`.
#' @return character vector of family labels (same length as `model`), `NA`
#'   for any model not recognised.
#' @noRd
wbal_evap_family <- function(model) {
  family_map <- c(dy_cd = "glm_aed", glm_aed = "glm_aed",
                  gotm_wet = "gotm_wet", simstrat_aed2 = "simstrat_aed2")
  unname(family_map[model])
}

#' Resolve a possibly-legacy `params` value down to one family's C/h_inv
#'
#' `water_balance()$params` (and `calc_water_balance()`'s `params` arg) may
#' be: `NULL` (nothing fitted yet); a flat legacy `c(C=, h_inv=)` vector from
#' before per-family storage was added, which applied uniformly to every
#' model; or the current family-keyed list (`list(glm_aed = c(...), ...)`).
#'
#' @param params `NULL`, a flat legacy vector, or a family-keyed list.
#' @param family character; single family label to resolve, e.g. from
#'   `wbal_evap_family()`.
#' @return A `c(C=, h_inv=)` vector, or `NULL` if nothing is set for `family`.
#' @noRd
resolve_wbal_params <- function(params, family) {
  if (is.null(params)) return(NULL)
  if (is.list(params)) return(params[[family]])
  params
}
