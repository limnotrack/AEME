#' Read and write AED parameter CSV files
#' These functions handle reading and writing AED parameter CSV files,
#' ensuring that single quotes are properly managed in column names and
#' specific columns.
#' @param file The path to the CSV file.
#' @param df A data frame to be written to CSV.
#' @return For `read_aed_param_csv`, a data frame read from the CSV file.
#' @importFrom readr read_csv write_csv
#' @export
read_aed_param_csv <- function(file) {
  # Check file exists
  if (!file.exists(file)) {
    cli::cli_abort(c("x" = "File {file} does not exist"))
  }
  
  df <- readr::read_csv(file, show_col_types = FALSE)
  
  # Strip "'" from column names and column which has name in the column name
  names(df) <- gsub("'", "", names(df))
  name_col <- names(df)[grepl("name", names(df))]
  if (length(name_col) > 0) {
    df[[name_col]] <- gsub("'", "", df[[name_col]])
  }
  return(df)
}

#' @export
#' @rdname read_aed_param_csv
write_aed_param_csv <- function(df, file) {
  # Add "'" to column names and column which has name in the column name
  names(df) <- paste0("'", names(df), "'")
  name_col <- names(df)[grepl("name", names(df))]
  if (length(name_col) > 0) {
    df[[name_col]] <- paste0("'", df[[name_col]], "'")
  }
  readr::write_csv(df, file)
}
