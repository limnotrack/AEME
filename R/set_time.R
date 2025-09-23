#' Set time parameters for an Aeme object
#'
#' @inheritParams build_aeme
#' @param start,stop Time in the format "YYYY-mm-dd" or "YYYY-mm-dd HH:MM" or 
#' "YYYY-mm-dd HH:MM:SS"
#' @param spin_up Spin-up time in days. Can be a single numeric value or a list with
#' model names as names and numeric values as values.
#'
#' @returns Aeme object with time parameters set
#' @export
#'
#' @examples
#' aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
#' aeme <- readRDS(aeme_file)
#' aeme <- set_time(aeme = aeme, start = "2020-01-01", stop = "2020-12-31",
#'                  spin_up = 35)

set_time <- function(aeme, start, stop, spin_up) {
  # Check if aeme is a Aeme object
  aeme <- check_aeme(aeme)
  model <- list_models()
  aeme_time <- time(aeme)
  # Check if start and stop are of the format "YYYY-MM-DD HH:MM:SS"
  if (!missing(start)) {
    start <- check_time_format(start)
    aeme_time$start <- start
  }
  if (!missing(stop)) {
    stop <- check_time_format(stop)
    aeme_time$stop <- stop
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
