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
#' @inheritParams build_aeme
#' @param C numeric; outflow coefficient. Scales the magnitude of outflow when
#'   water level exceeds \code{h_inv}.
#' @param h_inv numeric; inversion height (m). The water level threshold below
#'   which outflow is zero.
#' @param params Optional named numeric vector with elements \code{"C"} and
#'   \code{"h_inv"}, as returned by \code{\link{get_wbal_param}}. If supplied,
#'   overrides the individual \code{C} and \code{h_inv} arguments.
#'
#' @returns An `Aeme` object with updated water balance parameters.
#' @importFrom cli cli_abort
#' @export

set_wbal_param <- function(aeme, C, h_inv, params = NULL) {
  aeme <- check_aeme(aeme)
  wbal <- aeme |> 
    water_balance()
  
  if (!is.null(params)) {
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
  
  wbal[["params"]] <- c(C = C, h_inv = h_inv)
  water_balance(aeme) <- wbal
  return(aeme)
}
