## code to prepare `aeme_parameters_bgc` dataset goes here
library(AEME)
param_names <- param_colnames()
aeme_parameters_bgc <- read.csv("data-raw/aeme_parameters_bgc.csv") |>
  dplyr::mutate(group = NA_character_) |>
  dplyr::select(dplyr::any_of(c(param_names))) |> 
  tibble::as_tibble()

usethis::use_data(aeme_parameters_bgc, overwrite = TRUE)
