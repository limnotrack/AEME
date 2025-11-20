#' Read GOTM hypsometry file
#'
#' @param file Path to GOTM hypsometry file
#'
#' @returns Data frame with columns "depth" and "area"
#' @noRd
#'
read_gotm_hyps <- function(file) {
  
  # Read lines
  lines <- readLines(file)
  
  # Parse header: "52 2"
  header <- lines[[1]] |>
    strsplit("\\s+") |>
    (\(x) as.numeric(x[[1]]))()
  
  n_levels <- header[1]
  
  # Read remaining lines into a data frame
  out <- lines[-1] |>
    (\(x) paste(x, collapse = "\n"))() |>
    (\(x) read.table(
      text = x,
      col.names = c("depth", "area"),
      colClasses = c("numeric", "numeric")
    ))()
  
  # Optional check
  if (nrow(out) != n_levels) {
    warning("Number of rows (", nrow(out),
            ") does not match header n_levels = ", n_levels)
  }
  
  out
}

