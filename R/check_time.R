#' Check if the time period is included in the data
#'
#' @param df dataframe; to check if the time period is included
#' @param model character; model name
#' @param aeme_time list; a list of start, stop and spin-up period for each
#' model from aeme_data object
#'
#' @return vector; of boolean values for if model spin-up period is included in
#' the data
#' @noRd
#'

check_time <- function(df, model, aeme_time, name = "") {
  names(model) <- model
  spin_chk <- sapply(model, \(m) {
    spin_start <- aeme_time[["start"]] - lubridate::ddays(aeme_time[["spin_up"]][[m]])
    spin_start %in% df[["Date"]]
  })
  start_chk <- aeme_time[["start"]] %in% df[["Date"]]
  stop_chk <- aeme_time[["stop"]] %in% df[["Date"]]

  if (any(!c(spin_chk, start_chk, stop_chk))) {
    chk <- list("spin up" = spin_chk, "start" = start_chk, "stop" = stop_chk)
    chk_dates <- list("spin up" = NULL, "start" = as.character(aeme_time[["start"]]),
                      "stop" = as.character(aeme_time[["stop"]]))
    missing_dates <- names(chk)[!sapply(chk, any)]

    msg <- c()
    if ("spin up" %in% missing_dates) {
      msg <- paste0(msg, "Spin up date is not included in the ", name, "data for ",
                    paste0(names(spin_chk)[!spin_chk], collapse = ", "), sep = "\n")
    }
    if (any(c("start", "stop") %in% missing_dates)) {
      msg <- paste0(msg, paste0(missing_dates, " date ", chk_dates[missing_dates],
                                 " is not included in the ", name, " data.", sep = "\n"), collapse = "\n")
    }
    stop(msg)
  }
}
