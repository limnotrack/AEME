#' Check and convert time input to POSIXct
#'
#' @param x character, Date, or POSIXt object representing time
#' @param tz character; timezone, default is "UTC"
#'
#' @returns POSIXct object
#' @noRd

check_time_format <- function(x, tz = "UTC") {
  # If it's already Date or POSIXt, convert to POSIXct
  if (inherits(x, "Date")) {
    return(as.POSIXct(x, tz = tz))
  } else if (inherits(x, "POSIXt")) {
    return(as.POSIXct(x, tz = tz))
  } else if (is.character(x)) {
    # Acceptable formats
    formats <- c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d")
    parsed <- NA
    for (fmt in formats) {
      parsed <- as.POSIXct(x, format = fmt, tz = tz)
      if (!is.na(parsed)) return(parsed)
    }
    stop("Input must be in 'YYYY-mm-dd' or 'YYYY-mm-dd HH:MM' or 'YYYY-mm-dd HH:MM:SS' format, or a Date/POSIXt object")
  } else {
    stop("Input must be a character string, Date, or POSIXt object")
  }
}
