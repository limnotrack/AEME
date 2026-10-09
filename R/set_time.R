#' Set time parameters for an Aeme object
#'
#' @inheritParams build_aeme
#' @param start,stop Time in the format "YYYY-mm-dd" or "YYYY-mm-dd HH:MM" or
#' "YYYY-mm-dd HH:MM:SS". Interpreted as wall-clock time in the object's
#' timezone (\code{time(aeme)$tz}, or \code{tz} if supplied here) and stored as
#' UTC.
#' @param spin_up Spin-up time in days. Can be a single numeric value or a list with
#' model names as names and numeric values as values.
#' @param tz character; Olson timezone in which user-supplied timestamps (here
#' and in the forcing/observation inputs) are expressed. Stored on the object as
#' \code{time$tz} and used to convert those timestamps to UTC internally and for
#' display. If omitted, the object's existing \code{time$tz} is kept.
#' @param time_step numeric; model integration time step in seconds. Default
#' (when unset on the object) 3600.
#' @param output_time_step numeric; model output time step in seconds. Must be
#' greater than or equal to \code{time_step}. Default (when unset on the object)
#' 86400 (daily). Set to e.g. 3600 for hourly output. Note that AEME does not
#' temporally disaggregate forcing: sub-daily output requires forcing supplied at
#' (at least) the same cadence.
#' @param output_daily_mean logical; if \code{TRUE}, every model additionally
#' produces a daily-mean output stream alongside its raw
#' \code{output_time_step} output. Default (when unset on the object)
#' \code{FALSE}. See \code{\link{set_output_time_step}}.
#'
#' @returns Aeme object with time parameters set
#' @export
#'
#' @examples
#' aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
#' aeme <- readRDS(aeme_file)
#' aeme <- set_time(aeme = aeme, start = "2020-01-01", stop = "2020-12-31",
#'                  spin_up = 35)

set_time <- function(aeme, start, stop, spin_up, time_step, output_time_step,
                     output_daily_mean, tz) {
  # Set timezone temporarily to UTC for all internal datetime arithmetic
  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")

  # Check if aeme is a Aeme object
  aeme <- check_aeme(aeme)
  model <- list_models()
  aeme_time <- time(aeme)
  if (is.null(aeme_time$time_step)) aeme_time$time_step <- 3600
  if (is.null(aeme_time$output_time_step)) aeme_time$output_time_step <- 86400
  if (is.null(aeme_time$output_daily_mean)) aeme_time$output_daily_mean <- FALSE
  # Declared input timezone: default to the object's existing value, else UTC
  if (!missing(tz)) {
    if (length(tz) != 1L || is.na(tz) || !tz %in% OlsonNames()) {
      cli::cli_abort(
        c("{.arg tz} must be a single valid Olson timezone name.",
          "x" = "Got {.val {tz}}.",
          "i" = "See {.run OlsonNames()}."),
        class = "aeme_error_time_tz"
      )
    }
    aeme_time$tz <- tz
  }
  if (is.null(aeme_time$tz)) aeme_time$tz <- "UTC"
  # start/stop: a Date is a calendar day (midnight UTC, never shifted); a
  # character/POSIXct value is wall-clock time in aeme_time$tz -> stored UTC.
  .bound_to_utc <- function(v) {
    if (inherits(v, "Date")) {
      return(as.POSIXct(format(v, "%Y-%m-%d"), tz = "UTC"))
    }
    .to_utc(check_time_format(v, tz = aeme_time$tz), tz = aeme_time$tz,
            reinterpret_utc_tag = TRUE)
  }
  if (!missing(start)) aeme_time$start <- .bound_to_utc(start)
  if (!missing(stop))  aeme_time$stop  <- .bound_to_utc(stop)
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
  if (!missing(output_daily_mean)) {
    if (!is.logical(output_daily_mean) || length(output_daily_mean) != 1 ||
        is.na(output_daily_mean)) {
      cli::cli_abort("{.arg output_daily_mean} must be a single {.cls logical}.",
                     class = "aeme_error_output_daily_mean")
    }
    aeme_time$output_daily_mean <- output_daily_mean
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
