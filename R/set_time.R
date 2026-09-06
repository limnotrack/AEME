#' Set time parameters for an Aeme object
#'
#' @inheritParams build_aeme
#' @param start,stop Time in the format "YYYY-mm-dd" or "YYYY-mm-dd HH:MM" or
#' "YYYY-mm-dd HH:MM:SS"
#' @param spin_up Spin-up time in days. Can be a single numeric value or a list with
#' model names as names and numeric values as values.
#' @param time_step numeric; model integration time step in seconds. Default
#' (when unset on the object) 3600.
#' @param output_time_step numeric; model output time step in seconds. Must be
#' greater than or equal to \code{time_step}. Default (when unset on the object)
#' 86400 (daily). Set to e.g. 3600 for hourly output. Note that AEME does not
#' temporally disaggregate forcing: sub-daily output requires forcing supplied at
#' (at least) the same cadence.
#'
#' @returns Aeme object with time parameters set
#' @export
#'
#' @examples
#' aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
#' aeme <- readRDS(aeme_file)
#' aeme <- set_time(aeme = aeme, start = "2020-01-01", stop = "2020-12-31",
#'                  spin_up = 35)

set_time <- function(aeme, start, stop, spin_up, time_step, output_time_step) {
  # Check if aeme is a Aeme object
  aeme <- check_aeme(aeme)
  model <- list_models()
  aeme_time <- time(aeme)
  if (is.null(aeme_time$time_step)) aeme_time$time_step <- 3600
  if (is.null(aeme_time$output_time_step)) aeme_time$output_time_step <- 86400
  # Check if start and stop are of the format "YYYY-MM-DD HH:MM:SS"
  if (!missing(start)) {
    start <- check_time_format(start)
    aeme_time$start <- start
  }
  if (!missing(stop)) {
    stop <- check_time_format(stop)
    aeme_time$stop <- stop
  }
  if (!missing(time_step)) {
    if (!is.numeric(time_step) || length(time_step) != 1 || time_step <= 0) {
      cli::cli_abort("{.arg time_step} must be a single positive number (seconds).",
                     class = "aeme_error_time_step")
    }
    aeme_time$time_step <- time_step
  }
  if (!missing(output_time_step)) {
    if (!is.numeric(output_time_step) || length(output_time_step) != 1 ||
        output_time_step <= 0) {
      cli::cli_abort(paste("{.arg output_time_step} must be a single positive",
                           "number (seconds)."),
                     class = "aeme_error_output_time_step")
    }
    aeme_time$output_time_step <- output_time_step
  }
  if (aeme_time$output_time_step < aeme_time$time_step) {
    cli::cli_abort(
      c("{.arg output_time_step} must be >= {.arg time_step}.",
        "x" = "output_time_step: {.val {aeme_time$output_time_step}} s",
        "x" = "time_step: {.val {aeme_time$time_step}} s"),
      class = "aeme_error_output_time_step"
    )
  }
  if (!missing(spin_up)) {
    if (is.list(spin_up)) {
      
    } else if (is.numeric(spin_up)) {
      names(model) <- model
      spin_up <- lapply(model, \(x) spin_up)
    }
    aeme_time$spin_up <- spin_up
  }
  time(aeme) <- aeme_time
  return(aeme)
}
