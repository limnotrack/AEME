## code to prepare `aeme_parameters_bgc` dataset goes here
library(AEME)
param_names <- param_colnames()
aeme_parameters_bgc <- read.csv("data-raw/aeme_parameters_bgc.csv") |>
  dplyr::mutate(group = NA_character_) |>
  dplyr::select(dplyr::any_of(c(param_names))) |> 
  tibble::as_tibble()

aed2_params <- aeme_parameters_bgc |> 
  dplyr::filter(model == "glm_aed" & file != "glm3.nml")

aed2_param_names <- aed2_params |>
  dplyr::pull(name) |> 
  unique() |> 
  strsplit("/") |>
  sapply(\(x) x[2]) |> 
  unique() |> 
  tolower()

upd_param <- glm_aed_parameters |> 
  dplyr::filter(grepl(paste0(aed2_param_names, collapse = "|"), name),
                !is.na(value), file == "aed.nml")
aeme_parameters_bgc <- aeme_parameters_bgc |>
  dplyr::filter(!(model == "glm_aed" & name %in% aed2_params$name)) |>
  dplyr::bind_rows(upd_param) |>
  dplyr::arrange(model, file, name)


usethis::use_data(aeme_parameters_bgc, overwrite = TRUE)
