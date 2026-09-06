#' Check if the time period is included in the data
#'
#' @param df dataframe; to check if the time period is included
#' @param model character; model name
#' @param aeme_time list; a list of start, stop and spin-up period for each
#' model from aeme object
#' @param name character; name of the data frame being checked (for error 
#' messages)
#'
#' @importFrom lubridate ddays
#'
#' @return vector; of logical values for if model spin-up period is included in
#' the data
#' @noRd
#'

check_time <- function(df, model, aeme_time, name = "") {
  date_col <- "Date"
  if (!date_col %in% colnames(df)) {
    # Detect a "Date" column
    col_classes <- sapply(df, class)
    date_col <- names(col_classes)[sapply(col_classes, function(c) any(c %in% c("Date", "POSIXct", "POSIXt")))]
    if (length(date_col) == 0) {
      # cli::cli_abort("{.arg df} must contain a {.var Date} column.")
      cli::cli_abort(c(
        "{.arg df} must contain a {.cls Date} column.",
        "i" = "No date column detected. Please ensure your data frame has a column of class {.cls Date} or {.cls POSIXct}."
      ), class = "aeme_error_missing_date_column")
    }
  }

  # Coverage is checked on the calendar-day grid so that daily forcing still
  # satisfies a sub-daily simulation window (AEME does not disaggregate, but a
  # daily series does bracket every sub-daily timestamp within a day).
  df_dates <- as.Date(df[[date_col]])
  data_min <- min(df_dates, na.rm = TRUE)
  data_max <- max(df_dates, na.rm = TRUE)

  # Compute spin-up dates (earliest date each model needs)
  spin_dates <- compute_spinup_dates(model, aeme_time)
  spin_chk <- spin_dates >= data_min & spin_dates <= data_max

  # Start and stop checks -- the simulation window must sit inside the data
  start_chk <- as.Date(aeme_time[["start"]]) >= data_min
  stop_chk  <- as.Date(aeme_time[["stop"]])  <= data_max

  # Collect missing checks
  missing <- c(
    spin_up = any(!spin_chk),
    start   = !start_chk,
    stop    = !stop_chk
  )

  if (any(missing)) {
    msgs <- c()

    # Spin-up messages
    if (missing["spin_up"]) {
      msgs <- c(msgs, paste0(
        "Spin-up date(s) for model(s) ",
        paste(names(spin_chk)[!spin_chk], collapse = ", "),
        " are outside the ", name, " data (data covers ", data_min, " to ",
        data_max, ")."
      ))
    }

    # Start/stop messages
    for (d in c("start", "stop")) {
      if (missing[d]) {
        msgs <- c(msgs, paste0(
          toupper(substr(d, 1, 1)), substr(d, 2, nchar(d)),
          " date ", as.character(aeme_time[[d]]),
          " is outside the ", name, " data (data covers ", data_min, " to ",
          data_max, ")."
        ))
      }
    }

    cli::cli_abort(msgs, class = "aeme_error_missing_dates")
  }

  # Cadence check: sub-daily output requires forcing supplied at (at least)
  # that cadence -- AEME will not temporally disaggregate.
  ots <- aeme_time[["output_time_step"]]
  if (!is.null(ots) && is.finite(ots) && ots < 86400 && nrow(df) > 2) {
    ordered_t <- sort(as.POSIXct(df[[date_col]], tz = "UTC"))
    med_step <- stats::median(as.numeric(diff(ordered_t), units = "secs"),
                              na.rm = TRUE)
    if (is.finite(med_step) && med_step > ots) {
      cli::cli_abort(c(
        "The {name} data is coarser than the requested output time step.",
        "x" = "Forcing step: ~{round(med_step)} s; output_time_step: {ots} s.",
        "i" = paste("Supply {name} forcing at {ots} s or finer -- AEME does",
                    "not temporally disaggregate forcing.")
      ), class = "aeme_error_forcing_cadence")
    }
  }

  invisible(TRUE)
}

#' Compute spin-up start dates for models
#'
#' @param models character vector of model codes
#' @param aeme_time list with "start" (POSIXct/Date) and "spin_up" named by model
#' @return named Date vector of spin-up start dates
#' @noRd
compute_spinup_dates <- function(models, aeme_time) {
  names(models) <- models
  spin_dates <- lapply(models, function(m) {
    as.Date(aeme_time[["start"]]) - lubridate::ddays(aeme_time[["spin_up"]][[m]])
  })
  spin_dates <- as.Date(unlist(spin_dates))
  names(spin_dates) <- models
  spin_dates
}



