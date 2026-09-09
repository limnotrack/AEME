#' Build a model time axis (POSIXct) from an AEME time list
#'
#' Internal helper that reconstructs the sequence of timestamps a model
#' either is forced with or writes output at, honouring the configured
#' \code{time_step} / \code{output_time_step} rather than assuming a daily
#' cadence.
#'
#' The sequence starts at the model's spin-up start
#' (\code{start - spin_up[[model]]} days) and runs to \code{stop}, stepped by
#' the requested resolution in seconds. For \code{glm_aed} the first timestamp
#' is dropped because GLM does not write output for the initial state.
#'
#' @param aeme_time list; the \code{time} slot of an Aeme object.
#' @param model character; single model code (e.g. \code{"glm_aed"}).
#' @param which character; \code{"output"} (default) uses
#'   \code{output_time_step}, \code{"forcing"} uses \code{time_step}.
#' @param remove_spin_up logical; if \code{TRUE}, also return the index of the
#'   timestamps on or after \code{start} (the post-spin-up window).
#'
#' @return list with \code{axis} (POSIXct vector) and \code{index} (integer
#'   positions into \code{axis}; all positions when
#'   \code{remove_spin_up = FALSE}).
#' @noRd
aeme_time_axis <- function(aeme_time, model, which = c("output", "forcing"),
                           remove_spin_up = TRUE) {
  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")
  which <- match.arg(which)
  step <- if (which == "output") {
    aeme_time[["output_time_step"]] %||% 86400
  } else {
    aeme_time[["time_step"]] %||% 3600
  }
  step <- as.numeric(step)

  spin_up_days <- aeme_time[["spin_up"]][[model]]
  if (is.null(spin_up_days) || is.na(spin_up_days)) spin_up_days <- 0

  start <- as.POSIXct(aeme_time[["start"]], tz = "UTC")
  stop <- as.POSIXct(aeme_time[["stop"]], tz = "UTC")
  sim_start <- start - spin_up_days * 86400

  axis <- seq(from = sim_start, to = stop, by = step)

  if (model == "glm_aed") {
    # GLM does not output on the first date / first step
    axis <- axis[-1]
    # number of output steps that fall within the spin-up period
    n_spin <- spin_up_days * 86400 / step
  } else {
    n_spin <- spin_up_days * 86400 / step
  }

  index <- seq_along(axis)
  if (remove_spin_up) {
    index <- index[index > n_spin]
  }

  list(axis = axis, index = index)
}
