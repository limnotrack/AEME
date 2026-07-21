#' Get water balance parameters
#'
#' Retrieves the outflow parameters from an `Aeme` object, as set by
#' \code{\link{set_wbal_param}}. See that function for details of how the
#' parameters are used in the outflow equation.
#'
#' A typical use case is calibrating parameters over a period with observed
#' water level data, then transferring them to a period without:
#'
#' \enumerate{
#'   \item Build an \code{Aeme} object for a calibration period with observed
#'     water level data (e.g. 2010--2020) using \code{\link{build_aeme}}.
#'   \item Run the water balance and retrieve the fitted parameters:
#'     \code{wbal_param <- get_wbal_param(aeme)}.
#'   \item Configure a new \code{Aeme} object for the target period without
#'     observed water levels (e.g. 2020--2024).
#'   \item Transfer the parameters with \code{\link{set_wbal_param}}:
#'     \code{aeme <- set_wbal_param(aeme, params = wbal_param)}.
#'   \item Build the new object with \code{\link{build_aeme}}.
#' }
#'
#' @inheritParams build_aeme
#'
#' @returns A named numeric vector with elements \code{C} (outflow coefficient)
#'   and \code{h_inv} (inversion height, m), or \code{NULL} if no parameters
#'   have been set.
#' @seealso \code{\link{set_wbal_param}}, \code{\link{reset_wbal_param}}
#' @export

get_wbal_param <- function(aeme) {
  check_aeme(aeme)
  wbal <- aeme |> 
    water_balance()
  
  params <- wbal[["params"]]
  
  if (is.null(params)) {
    cli_inform_safe(c("!" = "No water balance parameters found in Aeme object."))
  }
  return(params)
}
