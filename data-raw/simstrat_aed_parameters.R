library(AEME)

# Physical (hydrodynamic) parameters from the Simstrat-AED par template.
# Mirrors simstrat_aed2_parameters.R -- Simstrat's own physical parameters
# don't depend on which BGC library it's coupled to.
par <- jsonlite::fromJSON("inst/extdata/simstrat_aed/simstrat.par",
                         simplifyVector = FALSE)
mp <- par[["ModelParameters"]]

phys_pars <- c("a_seiche", "a_seiche_w", "f_wind", "c10", "cd", "hgeo",
              "p_air", "p_sw_water", "p_lw", "p_windf", "p_absorb",
              "beta_sol", "wat_albedo", "q_nn")

phys_df <- data.frame(
  model = "simstrat_aed",
  file = "simstrat.par",
  name = paste0("ModelParameters/", phys_pars),
  value = sapply(phys_pars, \(p) as.numeric(mp[[p]])),
  par = phys_pars,
  module = "hydrodynamic",
  row.names = NULL
)

# AED (v3) biogeochemical initial-concentration parameters from the shared
# aed.nml template (the same one initialise_aed()/initialise_simstrat_aed()
# both write to) -- module set matches .aed_module_deps (R/aed_modules.R):
# no aed_carbon split the way AED2 has it.
aed_nml <- read_nml("inst/extdata/aed/aed.nml")
aed_pars <- list(
  aed_oxygen      = "oxy_initial",
  aed_silica      = "rsi_initial",
  aed_nitrogen    = c("amm_initial", "nit_initial"),
  aed_phosphorus  = "frp_initial",
  aed_organic_matter = c("poc_initial", "doc_initial", "pon_initial",
                         "don_initial", "pop_initial", "dop_initial")
)

aed_df <- do.call(rbind, lapply(names(aed_pars), \(block) {
  pars <- aed_pars[[block]]
  data.frame(
    model = "simstrat_aed",
    file = "aed.nml",
    name = paste0(block, "/", pars),
    value = sapply(pars, \(p) as.numeric(get_nml_value(aed_nml, p))),
    par = pars,
    module = "bgc",
    row.names = NULL
  )
}))

param_colnames <- param_colnames()
simstrat_aed_parameters <- dplyr::bind_rows(phys_df, aed_df) |>
  dplyr::mutate(
    min = 0.5 * value,
    max = 1.5 * value,
    index = NA_integer_,
    group = NA_character_
  ) |>
  dplyr::select(dplyr::any_of(param_colnames)) |>
  dplyr::select(-par) |>
  tibble::as_tibble()

usethis::use_data(simstrat_aed_parameters, overwrite = TRUE)
