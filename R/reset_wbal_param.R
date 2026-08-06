#' Reset water balance parameters
#'
#' This function resets the water balance parameters in the Aeme object. This is
#' useful if you want to start fresh with a new set of parameters for example
#' if you add/remove a inflow or change the meteorological data.
#'
#' @inheritParams build_aeme
#' @param model character; model name(s) to reset parameters for (e.g.
#'   \code{"glm_aed"}). If \code{NULL} (default), all fitted parameters are
#'   cleared.
#'
#' @returns Aeme object with water balance parameters reset
#' @export
#'

reset_wbal_param <- function(aeme, model = NULL) {
  aeme <- check_aeme(aeme)
  wbal <- aeme |>
    water_balance()

  if (is.null(model) || !is.list(wbal[["params"]])) {
    wbal[["params"]] <- NULL
  } else {
    model <- check_model(model = model)
    families <- unique(stats::na.omit(wbal_evap_family(model)))
    for (family in families) {
      wbal[["params"]][[family]] <- NULL
    }
    if (length(wbal[["params"]]) == 0) wbal[["params"]] <- NULL
  }

  water_balance(aeme) <- wbal
  return(aeme)
}
