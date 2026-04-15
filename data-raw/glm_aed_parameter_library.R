csv_files <- c(
  "data-raw/glm_parameter_library.csv",
  "data-raw/aed_parameter_library.csv"
)


glm_aed_parameter_library <- readr::read_csv(csv_files, show_col_types = FALSE)
usethis::use_data(glm_aed_parameter_library, overwrite = TRUE)
