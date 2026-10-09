library(AEME)
param_names <- param_colnames()
aeme_parameters <- read.csv("data-raw/aeme_parameters.csv") |>
  dplyr::mutate(group = NA_character_)|>
  dplyr::select(dplyr::any_of(c(param_names))) |> 
  tibble::as_tibble()
met_pars <- aeme_parameters |>
  dplyr::filter(file == "met") |>
  dplyr::distinct(name, .keep_all = TRUE) |> 
  dplyr::mutate(model = "simstrat_aed2")
sim_pars <- simstrat_aed2_parameters |> 
  dplyr::filter(grepl("a_seiche$|hgeo|cd", name))

aeme_parameters <- aeme_parameters |> 
  dplyr::bind_rows(met_pars, sim_pars) |> 
  dplyr::arrange(model, file, name) |> 
  dplyr::select(dplyr::any_of(c(param_names)))

usethis::use_data(aeme_parameters, overwrite = TRUE)
