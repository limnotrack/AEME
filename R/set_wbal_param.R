#' Set water balance parameters
#'
#' Sets the outflow parameters used in the lake water balance. Outflow is
#' calculated at each timestep as:
#'
#' \deqn{O_t = C \cdot \max(h_t - h_{inv}, 0)^{1.5} \times 86400}
#'
#' where \eqn{O_t} is outflow (m\eqn{^3}/day), \eqn{h_t} is the simulated
#' water level (m), \eqn{h_{inv}} is the inversion height (m), \eqn{C} is the
#' outflow coefficient, and 86400 converts seconds to days.
#'
#' Parameters are stored per evaporation family, since `dy_cd`/`glm_aed`
#' share one fitted set and `gotm_wet`/`simstrat_aed2` each have their own
#' (see \code{\link{calc_water_balance}}). Use \code{model} to set a
#' specific model's parameters; omit it to apply the same values to every
#' family (matching the pre-per-family behaviour).
#'
#' @inheritParams build_aeme
#' @param C numeric; outflow coefficient. Scales the magnitude of outflow when
#'   water level exceeds \code{h_inv}.
#' @param h_inv numeric; inversion height (m). The water level threshold below
#'   which outflow is zero.
#' @param params Optional named numeric vector with elements \code{"C"} and
#'   \code{"h_inv"}, as returned by \code{\link{get_wbal_param}} for a single
#'   model. If supplied, overrides the individual \code{C} and \code{h_inv}
#'   arguments. Alternatively, a family-keyed list as returned by
#'   \code{\link{get_wbal_param}} with no \code{model} -- each entry is
#'   applied directly to its own family, and \code{model} is ignored.
#' @param model character; model name(s) to set parameters for (e.g.
#'   \code{"glm_aed"}). If \code{NULL} (default), the same values are applied
#'   to every evaporation family (\code{dy_cd}/\code{glm_aed}, \code{gotm_wet},
#'   \code{simstrat_aed2}). Ignored if \code{params} is a family-keyed list.
#'
#' @returns An `Aeme` object with updated water balance parameters.
#' @importFrom cli cli_abort
#' @export

set_wbal_param <- function(aeme, C, h_inv, params = NULL, model = NULL) {
  aeme <- check_aeme(aeme)
  wbal <- aeme |>
    water_balance()

  existing <- wbal[["params"]]
  # Start fresh (or upgrade a legacy flat vector) into the family-keyed list
  wbal[["params"]] <- if (is.list(existing)) existing else list()

  # Family-keyed list (e.g. straight from get_wbal_param(aeme)) -- apply
  # each entry to its own family directly. A plain list(C=, h_inv=) is
  # treated as the flat-pair form below instead of two bogus families.
  if (is.list(params) && !identical(sort(names(params)), c("C", "h_inv"))) {
    for (family in names(params)) {
      wbal[["params"]][[family]] <- params[[family]]
    }
    water_balance(aeme) <- wbal
    return(aeme)
  }

  if (!is.null(params)) {
    if (is.list(params)) params <- unlist(params)
    missing_names <- setdiff(c("C", "h_inv"), names(params))
    if (length(missing_names) > 0) {
      cli::cli_abort(c(
        "{.arg params} must contain {.field C} and {.field h_inv}.",
        "x" = "Missing: {.field {missing_names}}"
      ))
    }
    C <- params[["C"]]
    h_inv <- params[["h_inv"]]
  }

  new_params <- c(C = C, h_inv = h_inv)

  families <- if (is.null(model)) {
    unique(stats::na.omit(wbal_evap_family(list_models())))
  } else {
    model <- check_model(model = model)
    unique(stats::na.omit(wbal_evap_family(model)))
  }

  for (family in families) {
    wbal[["params"]][[family]] <- new_params
  }

  water_balance(aeme) <- wbal
  return(aeme)
}
