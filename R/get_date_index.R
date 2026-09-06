#' Get date index for each model in the AEME object
#'
#' @inheritParams get_var
#'
#' @returns A list with date index for each model
#' @export
#'
get_date_index <- function(aeme, model, remove_spin_up = TRUE) {
  aeme_time <- time(aeme)
  if (missing(model)) {
    model <- list_models(aeme)
  } else {
    model <- check_model(model = model)
  }
  date_index <- lapply(model, \(m) {
    aeme_time_axis(aeme_time = aeme_time, model = m, which = "output",
                   remove_spin_up = remove_spin_up)[["index"]]
  })
  names(date_index) <- model
  return(date_index)
}
