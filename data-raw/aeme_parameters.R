param_names <- AEME:::get_param_names()
aeme_parameters <- read.csv("data-raw/aeme_parameters.csv") |>
  dplyr::mutate(group = NA)|>
  dplyr::select(dplyr::all_of(c(param_names)))

usethis::use_data(aeme_parameters, overwrite = TRUE)
