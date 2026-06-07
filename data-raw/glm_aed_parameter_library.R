csv_files <- c(
  "data-raw/glm_parameter_library.csv",
  "data-raw/aed_parameter_library.csv",
  "data-raw/aed_organic_matter_pars.csv",
  "data-raw/aed_phyto_pars.csv",
  "data-raw/aed_zoop_pars.csv",
  "data-raw/aed_macrophyte_pars.csv"
)

glm_aed_parameter_library <- readr::read_csv(csv_files, show_col_types = FALSE) |> 
  dplyr::mutate(
    group = tolower(group),
    parameter = dplyr::case_when(
      grepl("Fsed", parameter) ~ tolower(parameter),
      grepl("Ksed", parameter) ~ tolower(parameter),
      grepl("R_nitrif", parameter) ~ "rnitrif",
      grepl("K_nitrif", parameter) ~ "knitrif",
      grepl("R_denit", parameter) ~ "rdenit",
      grepl("K_denit", parameter) ~ "kdenit",
      
      .default = parameter
    )
  )

View(glm_aed_parameter_library)
glm_aed_parameter_library[grepl("doc", glm_aed_parameter_library$parameter), ] 

usethis::use_data(glm_aed_parameter_library, overwrite = TRUE)
