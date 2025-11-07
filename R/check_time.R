#' Check if the time period is included in the data
#'
#' @param df dataframe; to check if the time period is included
#' @param model character; model name
#' @param aeme_time list; a list of start, stop and spin-up period for each
#' model from aeme object
#'
#' @importFrom lubridate ddays
#'
#' @return vector; of logical values for if model spin-up period is included in
#' the data
#' @noRd
#'

check_time <- function(df, model, aeme_time, name = "") {
  if (!"Date" %in% colnames(df)) {
    cli::cli_abort("{.arg df} must contain a {.var Date} column.")
  }
  
  # Compute spin-up dates
  spin_dates <- compute_spinup_dates(model, aeme_time)
  spin_chk <- spin_dates %in% df[["Date"]]
  
  # Start and stop checks
  start_chk <- as.Date(aeme_time[["start"]]) %in% df[["Date"]]
  stop_chk  <- as.Date(aeme_time[["stop"]])  %in% df[["Date"]]
  
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
        " are missing from the ", name, " data."
      ))
    }
    
    # Start/stop messages
    for (d in c("start", "stop")) {
      if (missing[d]) {
        msgs <- c(msgs, paste0(
          toupper(substr(d, 1, 1)), substr(d, 2, nchar(d)),
          " date ", as.character(aeme_time[[d]]),
          " is missing from the ", name, " data."
        ))
      }
    }
    
    cli::cli_abort(msgs, class = "aeme_error_missing_dates")
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



