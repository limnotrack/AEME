# Simstrat-AED2 parameter library: physical (hydrodynamic) parameters from the
# Simstrat User Manual, combined with AED2 biogeochemical parameters.
#
# Simstrat-AED2 and GLM-AED both couple to the same AED2 library
# (libaed2 -- confirmed by AEME's bundled `aed2_phyto_pars.nml`/
# `aed2_zoop_pars.nml` being byte-identical between
# inst/extdata/glm_aed/ and inst/extdata/simstrat_aed2/), so the
# biogeochemical parameter descriptions already curated for
# `glm_aed_parameter_library` (oxygen, silica, nitrogen, phosphorus, organic
# matter, phytoplankton, zooplankton) apply directly here. Only the module
# name prefix differs ("aed_" in the historical AED-science documentation
# vs. "aed2_" in the actual `aed2.nml`/`aed2_phyto_pars.nml`/
# `aed2_zoop_pars.nml` files used by both models).
bgc_modules <- c("aed_oxygen", "aed_silica", "aed_nitrogen", "aed_phosphorus",
                 "aed_organic_matter", "aed_phytoplankton", "aed_zooplankton")

aed2_bgc <- glm_aed_parameter_library |>
  dplyr::filter(module %in% bgc_modules) |>
  dplyr::mutate(module = sub("^aed_", "aed2_", module))

# Carbon (aed2_carbon) has no AED (v1) analogue, so it is documented directly
# from AEME's bundled aed2.nml template.
aed2_carbon <- readr::read_csv("data-raw/aed2_carbon_parameter_library.csv",
                               show_col_types = FALSE)

# Simstrat physical (hydrodynamic) parameters, sourced from Table 1 of the
# Simstrat User Manual (SIMSTRAT_V304_UserManual.pdf) -- defaults are the
# values in AEME's bundled `inst/extdata/simstrat_aed2/simstrat.par` template.
simstrat_phys <- readr::read_csv("data-raw/simstrat_parameter_library.csv",
                                 show_col_types = FALSE)

simstrat_aed2_parameter_library <- dplyr::bind_rows(
  simstrat_phys |> dplyr::mutate(default = as.character(default)),
  aed2_carbon |> dplyr::mutate(default = as.character(default)),
  aed2_bgc |> dplyr::mutate(default = as.character(default))
) |>
  tibble::as_tibble()

usethis::use_data(simstrat_aed2_parameter_library, overwrite = TRUE)
