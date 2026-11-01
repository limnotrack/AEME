#' Hash of everything `calc_water_balance()` depends on
#'
#' Used to decide whether the water balance stored in `water_balance(aeme)` is
#' still valid for the inputs of the current `build_aeme()` call. The previous
#' fit (`water_balance()$params`) is deliberately *not* part of the key - it is
#' an output of an earlier call, so including it would change the key every
#' time. `forced_params` is the exception: parameters imposed from outside the
#' fit (the GLM weir), which do change the result.
#'
#' @param ... the arguments passed to `calc_water_balance()`.
#' @param forced_params `NULL` or a named list of imposed `C` / `h_inv`.
#' @return a character hash.
#' @noRd
wbal_input_hash <- function(..., forced_params = NULL) {
  rlang::hash(list(
    version = as.character(utils::packageVersion("AEME")),
    inputs = list(...),
    forced_params = forced_params
  ))
}

#' Observed lake data as seen by the water balance, for hashing
#'
#' The water balance only uses observed water temperature (to fit the surface
#' temperature used for evaporation). Restricting to those rows, de-duplicated
#' and ordered, keeps the hash stable when `calc_lake_obs_deriv()` appends
#' derived variables on every build.
#' @noRd
wbal_hash_obs_lake <- function(obs_lake) {
  if (!is.data.frame(obs_lake) || !"var_aeme" %in% names(obs_lake)) {
    return(obs_lake)
  }
  obs <- obs_lake[obs_lake[["var_aeme"]] %in% "HYD_temp", , drop = FALSE]
  # row order is kept: the water balance takes the first record per day
  as.data.frame(dplyr::distinct(obs))
}

#' Is the stored water balance valid for this hash?
#'
#' @param w_bal the `water_balance(aeme)` list.
#' @param key hash from `wbal_input_hash()`.
#' @noRd
wbal_cache_valid <- function(w_bal, key) {
  is.data.frame(w_bal[["data"]][["wbal"]]) &&
    nrow(w_bal[["data"]][["wbal"]]) > 0 &&
    identical(w_bal[["hash"]], key)
}
