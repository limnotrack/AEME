#' Add meteorological data to Aeme object
#' 
#' @inheritParams build_aeme
#' @param met data frame with meteorological data. Must include columns "Date",
#' "MET_radswd", "MET_radswd", "MET_pprain" and "MET_wndspd" or "MET_wnduvu" and
#'  "MET_wnduvv".
#'
#' @returns Aeme object with meteorological data added
#' @export
#'

add_met <- function(aeme, met) {
  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")
  aeme <- check_aeme(aeme)
  inp <- aeme |>
    input()

  met <- check_met(met)
  # Ingest boundary: interpret the Date column in the object's declared input
  # timezone and store UTC (daily data is left as calendar dates). This is the
  # one place a "UTC"-tagged column is reinterpreted when tz is non-UTC, so
  # supply raw source data here -- not an already-ingested frame.
  tz <- time(aeme)[["tz"]] %||% "UTC"
  met[["Date"]] <- .as_forcing_datetime(met[["Date"]], tz = tz,
                                        reinterpret_utc_tag = TRUE)
  inp[["meteo"]] <- met
  input(aeme) <- inp
  return(aeme)
}
