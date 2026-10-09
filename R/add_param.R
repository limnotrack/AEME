#' Add model parameters to Aeme object
#'
#' @inheritParams build_aeme
#' @param param data frame with columns "model", "file", "name", "value", "min",
#' "max", "module" and "group.
#'
#' @returns Aeme object with parameters added
#' @export
#'

add_param <- function(aeme, param) {
  # Check if aeme is a Aeme object
  aeme <- check_aeme(aeme)
  
  if (!is.data.frame(param)) {
    stop("param must be a data frame")
  }
  
  if (!"group" %in% names(param)) {
    param$group <- NA
  }
  required_cols <- c("model", "file", "name", "value", "min", "max", "group")
  if (!all(required_cols %in% colnames(param))) {
    stop(paste0("param must have columns: ", 
                paste(required_cols, collapse = ", ")))
  }

  param_old <- parameters(aeme)
  if (nrow(param_old) == 0) {
    param_old <- data.frame()
  } else {
    # If duplicates remove them. A duplicate is the same model/file/name *and*
    # group/index, so adding one group's (or one vector element's) value does
    # not wipe the others. `index` only counts when both tables carry it.
    key_cols <- c("model", "file", "name", "group")
    if ("index" %in% names(param_old) && "index" %in% names(param) &&
        !all(is.na(param$index))) {
      key_cols <- c(key_cols, "index")
    }
    key_cols <- intersect(key_cols, intersect(names(param_old), names(param)))
    param_old <- dplyr::anti_join(param_old, param, by = key_cols)
  }
  param_new <- dplyr::bind_rows(param_old, param)

  parameters(aeme) <- param_new
  return(aeme)
}
