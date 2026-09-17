#' Get water balance parameters
#'
#' Retrieves the outflow parameters from an `Aeme` object, as set by
#' \code{\link{set_wbal_param}}. See that function for details of how the
#' parameters are used in the outflow equation.
#'
#' Parameters are stored per evaporation family, since `dy_cd`/`glm_aed`
#' share one fitted set and `gotm_wet`/`simstrat_aed2` each have their own
#' (see \code{\link{calc_water_balance}}). Use \code{model} to fetch a
#' specific model's parameters; omit it to get every family that has been
#' fitted so far.
#'
#' A typical use case is calibrating parameters over a period with observed
#' water level data, then transferring them to a period without:
#'
#' \enumerate{
#'   \item Build an \code{Aeme} object for a calibration period with observed
#'     water level data (e.g. 2010--2020) using \code{\link{build_aeme}}.
#'   \item Run the water balance and retrieve the fitted parameters:
#'     \code{wbal_param <- get_wbal_param(aeme, model = "glm_aed")}.
#'   \item Configure a new \code{Aeme} object for the target period without
#'     observed water levels (e.g. 2020--2024).
#'   \item Transfer the parameters with \code{\link{set_wbal_param}}:
#'     \code{aeme <- set_wbal_param(aeme, params = wbal_param, model = "glm_aed")}.
#'   \item Build the new object with \code{\link{build_aeme}}.
#' }
#'
#' @inheritParams build_aeme
#' @param model character; model name(s) to fetch parameters for (e.g.
#'   \code{"glm_aed"}). If \code{NULL} (default), returns every fitted
#'   family as a named list.
#'
#' @returns If \code{model} resolves to a single evaporation family, a named
#'   numeric vector with elements \code{C} (outflow coefficient) and
#'   \code{h_inv} (inversion height, m). If \code{model} is \code{NULL}, a
#'   named list of such vectors, keyed by family. \code{NULL} if no
#'   parameters have been set.
#' @seealso \code{\link{set_wbal_param}}, \code{\link{reset_wbal_param}}
#' @export

get_wbal_param <- function(aeme, model = NULL) {
  aeme <- check_aeme(aeme)
  wbal <- aeme |>
    water_balance()

  params <- wbal[["params"]]

  if (is.null(params)) {
    cli_inform_safe(c("!" = "No water balance parameters found in Aeme object."))
    return(NULL)
  }

  # Legacy flat c(C=, h_inv=) vector, from before per-family storage was
  # added -- applies uniformly regardless of `model`.
  if (!is.list(params)) {
    return(params)
  }

  if (is.null(model)) {
    return(params)
  }

  model <- check_model(model = model)
  families <- unique(stats::na.omit(wbal_evap_family(model)))
  out <- params[families]
  if (length(out) == 1) out[[1]] else out
}
