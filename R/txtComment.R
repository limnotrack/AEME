#' Insert text with a comment
#'
#' @param value value to be inserted
#' @param width width after for comment
#' @param comment string comment
#'
#' @return value with comment after it
#' @noRd

txtComment <- function(value, width, comment) {

  # expand the value, if a vector
  value <- paste0(value, collapse = " ")

  # check if width allows enough room
  width <- ifelse(width > nchar(value), width, nchar(value)+2)

  paste0(value,
         paste(replicate(width - nchar(value)," "), collapse = ""), comment)

}

