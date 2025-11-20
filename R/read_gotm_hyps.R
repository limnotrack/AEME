#' Read GOTM hypsometry file
#'
#' @param file Path to GOTM hypsometry file
#'
#' @returns Data frame with columns "depth" and "area"
#' @noRd
#'
read_gotm_hyps <- function(file) {
  
  # Read all lines
  lines <- readr::read_lines(file)
  
  # First line contains: n_levels   flag
  header <- lines[[1]] |>
    strsplit("\\s+") |>
    (\(x) x[[1]])() |>
    as.numeric()
  
  n_levels <- header[1]
  
  # Read remaining lines as a dataframe
  out <- lines[-1] |>
    (\(x) readr::read_table(
      file = I(x),
      col_names = c("depth", "area"),
      col_types = readr::cols(
        depth = readr::col_double(),
        area  = readr::col_double()
      )
    ))()
  
  # Optionally check number of rows
  if (nrow(out) != n_levels) {
    warning("Number of rows does not match header value n_levels = ", n_levels)
  }
  
  out
}
