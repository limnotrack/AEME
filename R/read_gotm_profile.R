#' Read GOTM profile file
#'
#' @param file Path to GOTM profile file
#'
#' @returns Data frame with columns "datetime", "depth_m" and "value"
#' @export
#'

read_gotm_profile <- function(file) {
  if (!file.exists(file)) {
    stop("File '", file, "' does not exist.\n")
  }
  lines <- readLines(file)
  i <- 1
  result <- list()
  
  while (i <= length(lines)) {
    header_parts <- strsplit(lines[i], "\\s+")[[1]]
    datetime <- as.POSIXct(
      paste(header_parts[1], header_parts[2]),
      format = "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    )
    n_records <- as.integer(header_parts[3])
    
    # Read the next n_records lines
    data_lines <- lines[(i + 1):(i + n_records)]
    df <- read.table(
      text = paste(data_lines, collapse = "\n"),
      col.names = c("depth_mid", "value"),
      colClasses = c("numeric", "numeric")
    )
    df$datetime <- datetime
    
    result[[length(result) + 1]] <- df
    i <- i + n_records + 1
  }
  
  result <- result |> 
    dplyr::bind_rows() |> 
    dplyr::select(datetime, depth_mid, value)
  return(result)
}
