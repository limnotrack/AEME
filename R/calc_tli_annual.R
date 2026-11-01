#' Hydrological year label for the TLI
#'
#' The TLI protocol calculates each trophic level index from annual averages,
#' which deseasonalises the data. A year starting in July (the default) keeps
#' the austral growing season within a single year.
#'
#' @param date Date or POSIXct vector.
#' @param year_start_month integer; first month of the year. Defaults to the
#'   option `AEME.tli_year_start_month`, or 7 (July) if unset.
#' @return integer vector; the calendar year in which the year ends.
#' @noRd
tli_year <- function(date,
                     year_start_month = getOption("AEME.tli_year_start_month",
                                                  7L)) {
  yr <- lubridate::year(date)
  if (year_start_month > 1L) {
    yr <- yr + as.integer(lubridate::month(date) >= year_start_month)
  }
  yr
}

#' Annual mean of a time series, held on every time step of that year
#'
#' Short simulations (less than a full year) simply use the mean of the steps
#' available, so a run is never left without a TLI.
#'
#' @param x numeric vector (one value per time step).
#' @param date Date or POSIXct vector, the same length as `x`.
#' @return numeric vector, the same length as `x`; `NA` for years where every
#'   value is missing.
#' @noRd
annual_mean_by_year <- function(x, date,
                                year_start_month = getOption(
                                  "AEME.tli_year_start_month", 7L)) {
  yr <- tli_year(date, year_start_month)
  out <- rep(NA_real_, length(x))
  for (y in unique(yr)) {
    i <- which(yr == y)
    if (all(is.na(x[i]))) next
    out[i] <- mean(x[i], na.rm = TRUE)
  }
  out
}
