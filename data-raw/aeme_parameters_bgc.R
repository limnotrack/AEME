## code to prepare `aeme_parameters_bgc` dataset goes here

param_names <- AEME:::get_param_names()
aeme_parameters_bgc <- read.csv("data-raw/aeme_parameters_bgc.csv") |>
  dplyr::mutate(group = NA) |>
  dplyr::select(dplyr::all_of(c(param_names)))

usethis::use_data(aeme_parameters_bgc, overwrite = TRUE)
