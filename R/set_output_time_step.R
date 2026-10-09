#' Set the model output temporal frequency
#'
#' A thin, generic wrapper around [set_time()] for the common case of only
#' wanting to change how often every model in the ensemble writes output. The
#' frequency applies to all models: `build_glm()`, `build_gotm()` and
#' `build_simstrat()` each translate `output_time_step` into their native
#' output-cadence setting (`nsave`, `output.yaml` `time_step`, and
#' `Output/Times` respectively), and the internal `aeme_time_axis()`
#' reconstructs the matching output time axis when the results are read back.
#'
#' AEME does not temporally disaggregate forcing, so requesting output more
#' frequent than daily is only meaningful when the meteorological (and, where
#' relevant, inflow/outflow) forcing is supplied at least as often. See
#' `vignette("hourly-vs-daily-met")`.
#'
#' ## Daily-mean output
#'
#' `daily_mean = TRUE` makes every model additionally produce a **daily-mean**
#' output stream alongside the raw `frequency` output, so a run can be done at
#' a sub-daily cadence (for accuracy, or to average out a diurnal cycle) while
#' the results compared against daily observations are true daily means rather
#' than instantaneous snapshots. It is modelled on GOTM's native `output_daily`
#' stream (`time_method: mean`, one record per day):
#'
#' - **GOTM-WET** writes the daily means itself (its `output_daily.nc`).
#' - **GLM-AED** and **Simstrat** have no native time-averaging, so AEME
#'   averages their sub-daily `output.nc` by calendar day after the run and
#'   writes a companion `output_daily.nc`. The raw sub-daily output is kept.
#'
#' The daily stream stores only the targeted variables --- the
#' `model_controls` `simulate` set plus the fixed set AEME's readers require
#' --- not every model variable.
#'
#' @param aeme An [Aeme-class] object.
#' @param frequency The desired output frequency. Either a single positive
#'   number of **seconds**, or a character string:
#'   - a named frequency: `"subdaily"`/`"hourly"` (3600 s), `"daily"`
#'     (86400 s), `"weekly"` (604800 s);
#'   - a `"<n> <unit>"` string, e.g. `"6 hours"`, `"30 min"`, `"1 day"`,
#'     `"900 sec"` (units: `sec`/`min`/`hour`/`day`/`week`, singular or
#'     plural).
#' @param daily_mean logical; if `TRUE`, also produce a daily-mean output
#'   stream (see Details). Default `FALSE`.
#'
#' @return The `aeme` object with `time(aeme)$output_time_step` (and, when
#'   supplied, `time(aeme)$output_daily_mean`) updated. `output_time_step` must
#'   be greater than or equal to `time(aeme)$time_step`; [set_time()] raises
#'   `aeme_error_output_time_step` otherwise.
#' @export
#'
#' @seealso [set_time()]
#'
#' @examples
#' aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
#' aeme <- set_output_time_step(aeme, "hourly")
#' time(aeme)$output_time_step
#' aeme <- set_output_time_step(aeme, "6 hours")
#' time(aeme)$output_time_step
#' aeme <- set_output_time_step(aeme, "hourly", daily_mean = TRUE)
#' time(aeme)$output_daily_mean
set_output_time_step <- function(aeme, frequency, daily_mean = FALSE) {
  secs <- .duration_to_seconds(frequency)
  if (!is.logical(daily_mean) || length(daily_mean) != 1 || is.na(daily_mean)) {
    cli::cli_abort("{.arg daily_mean} must be a single {.cls logical}.",
                   class = "aeme_error_output_daily_mean")
  }
  if (isTRUE(daily_mean) && secs >= 86400) {
    cli::cli_warn(c(
      "!" = paste("{.arg daily_mean} is {.val TRUE} but {.arg frequency}",
                  "is {.val {frequency}} ({secs} s): there is no sub-daily",
                  "output to average."),
      "i" = "Set {.arg frequency} to a sub-daily value (e.g. {.val hourly})."
    ), class = "aeme_warn_output_daily_mean")
  }
  set_time(aeme, output_time_step = secs, output_daily_mean = daily_mean)
}

#' Coerce a duration given as seconds or a human string to seconds
#'
#' @param x numeric seconds, or a character frequency (see
#'   [set_output_time_step()]).
#' @return numeric scalar; seconds.
#' @noRd
.duration_to_seconds <- function(x) {
  if (is.numeric(x)) {
    if (length(x) != 1 || !is.finite(x) || x <= 0) {
      cli::cli_abort(
        "{.arg frequency} must be a single positive number of seconds.",
        class = "aeme_error_output_time_step"
      )
    }
    return(as.numeric(x))
  }
  if (!is.character(x) || length(x) != 1 || is.na(x)) {
    cli::cli_abort(
      "{.arg frequency} must be a number of seconds or a frequency string.",
      class = "aeme_error_output_time_step"
    )
  }

  key <- tolower(trimws(x))
  named <- c(subdaily = 3600, hourly = 3600, daily = 86400,
             day = 86400, week = 604800, weekly = 604800)
  if (key %in% names(named)) return(unname(named[[key]]))

  unit_secs <- c(sec = 1, secs = 1, second = 1, seconds = 1,
                 min = 60, mins = 60, minute = 60, minutes = 60,
                 hour = 3600, hours = 3600, hr = 3600, hrs = 3600,
                 day = 86400, days = 86400,
                 week = 604800, weeks = 604800)
  m <- regmatches(key, regexec("^([0-9]*\\.?[0-9]+)\\s*([a-z]+)$", key))[[1]]
  if (length(m) == 3 && m[3] %in% names(unit_secs)) {
    val <- as.numeric(m[2]) * unit_secs[[m[3]]]
    if (is.finite(val) && val > 0) return(val)
  }

  cli::cli_abort(c(
    "Could not interpret {.arg frequency} = {.val {x}}.",
    "i" = paste("Use seconds, a named frequency ({.val hourly}, {.val daily},",
                "{.val weekly}), or a {.val <n> <unit>} string like",
                "{.val {'6 hours'}}.")
  ), class = "aeme_error_output_time_step")
}
