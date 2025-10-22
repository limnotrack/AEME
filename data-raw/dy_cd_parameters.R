# Read DYRESM parameter file
lines <- readLines("inst/extdata/dy_cd/dyresm3p1.par")

# Define helper to shorten parameter names
shorten_name <- function(x) {
  dplyr::case_when(
    stringr::str_detect(x, "bulk") ~ "bulk_cd",
    stringr::str_detect(x, "albedo") ~ "albedo",
    stringr::str_detect(x, "emissivity") ~ "emissivity",
    stringr::str_detect(x, "critical wind") ~ "crit_wind",
    stringr::str_detect(x, "time of day") ~ "output_time",
    stringr::str_detect(x, "bubbler") ~ "bubbler_coeff",
    stringr::str_detect(x, "buoyant plume") ~ "plume_coeff",
    stringr::str_detect(x, "shear production") ~ "eta_K",
    stringr::str_detect(x, "potential energy") ~ "eta_P",
    stringr::str_detect(x, "wind stirring") ~ "eta_S",
    stringr::str_detect(x, "effective surface area") ~ "surf_area_coeff",
    stringr::str_detect(x, "bbl dissipation") ~ "bbl_dissip_coeff",
    stringr::str_detect(x, "vertical mixing") ~ "vert_mix_coeff",
    TRUE ~ stringr::str_replace_all(stringr::str_to_lower(x), "[^a-z0-9]+", "_")
  )
}

# Process and format parameters
params <- tibble::tibble(raw = lines) |>
  dplyr::filter(!stringr::str_detect(raw, "^<#|^=|^Parameters")) |>
  dplyr::mutate(
    value = as.numeric(stringr::str_extract(raw, "[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?")),
    comment = stringr::str_trim(stringr::str_remove(raw, ".*#\\s*<autoCal>\\s*"))
  ) |>
  dplyr::filter(!is.na(value)) |>
  dplyr::mutate(line = dplyr::row_number() + 2) |>
  dplyr::mutate(
    short_name = shorten_name(comment),
    name = paste0(short_name, "/", line),
    model = "dy_cd",
    file = "dyresm3p1.par",
    par = short_name
  ) |>
  dplyr::select(model, file, name, value, par)

params


# DYRESM .cfg file text
cfg_text <- "
<#5>
! DYRESM-CAEDYM configuration file for wainamu
2020213                              # start date
335                                  # simulation length (days)
.FALSE.                              # run CAEDYM (.TRUE. or .FALSE.)
1                                    # output Interval (in days, -9999 for every time step)
1.31                                 # light extinction coefficient (m-1)
0.2                                  # minimum layer thickness
0.5                                  # maximum layer thickness
3600                                 # time Step (s)
3                                    # number of Output Selections
TEMPTURE DENSITY SALINITY
.FALSE.                              # activate bubbler (.TRUE. or .FALSE.)
.FALSE.                              # activate non-neutral atmospheric stability (.TRUE. or .FALSE.)
"

# Split into lines
lines <- strsplit(cfg_text, "\n")[[1]]

# Process into tibble
cfg_params <- tibble::tibble(raw = lines) |>
  dplyr::filter(
    !stringr::str_detect(raw, "^<#|^!|^\\s*$|TEMPTURE|DENSITY|SALINITY")
  ) |>
  dplyr::mutate(
    # Extract numeric or logical/text values
    value_raw = stringr::str_trim(stringr::str_extract(raw, "^[^#]+")),
    comment = stringr::str_trim(stringr::str_remove(raw, ".*#"))
  ) |>
  dplyr::filter(value_raw != "") |>
  dplyr::mutate(
    value = dplyr::case_when(
      stringr::str_detect(value_raw, "\\.TRUE\\.") ~ TRUE,
      stringr::str_detect(value_raw, "\\.FALSE\\.") ~ FALSE,
      TRUE ~ suppressWarnings(as.numeric(value_raw))
    ),
    line = dplyr::row_number() + 2
  )

# Helper to shorten names
shorten_name_cfg <- function(x) {
  dplyr::case_when(
    stringr::str_detect(x, "start date") ~ "start_date",
    stringr::str_detect(x, "simulation length") ~ "sim_length_days",
    stringr::str_detect(x, "run CAEDYM") ~ "run_caedym",
    stringr::str_detect(x, "output Interval") ~ "output_interval_days",
    stringr::str_detect(x, "light extinction") ~ "light_extinction_coefficient",
    stringr::str_detect(x, "minimum layer") ~ "min_layer_thickness",
    stringr::str_detect(x, "maximum layer") ~ "max_layer_thickness",
    stringr::str_detect(x, "time Step") ~ "time_step_s",
    stringr::str_detect(x, "number of Output") ~ "n_output_selections",
    stringr::str_detect(x, "activate bubbler") ~ "activate_bubbler",
    stringr::str_detect(x, "non-neutral atmospheric") ~ "non_neutral_stability",
    TRUE ~ stringr::str_replace_all(stringr::str_to_lower(x), "[^a-z0-9]+", "_")
  )
}

# Final formatted dataframe
cfg_df <- cfg_params |>
  dplyr::mutate(
    short_name = shorten_name_cfg(comment),
    name = paste0(short_name, "/", line),
    model = "dy_cd",
    file = "cfg",
    par = short_name
  ) |>
  dplyr::select(model, file, name, value, par) |> 
  dplyr::slice(c(5:7))

dy_cd_parameters <- dplyr::bind_rows(params, cfg_df) |> 
  dplyr::mutate(
    min = 0.5 * value,
    max = 1.5 * value,
    module = "hydrodynamic",
    group = NA_character_
  ) |>
  dplyr::select(model, file, name, value, min, max, module, group, par)

usethis::use_data(dy_cd_parameters, overwrite = TRUE)
