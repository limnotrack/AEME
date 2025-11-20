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
    dt <- seq.Date(as.Date(aeme_time$start) - aeme_time$spin_up[[m]], 
             as.Date(aeme_time$stop), by = "day")
    if (m == "glm_aed") {
      # Adjust for glm_aed date issue - GLM does not output on the first date
      dt <- dt[-1]
    }
    idx <- seq_len(length(dt))
    if (remove_spin_up) {
      idx <- idx[idx > aeme_time$spin_up[[m]]]
    }
    return(idx)
  })
  names(date_index) <- model
  return(date_index)
}
