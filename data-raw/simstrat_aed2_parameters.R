library(AEME)

# Physical (hydrodynamic) parameters from the Simstrat par template
par <- jsonlite::fromJSON("inst/extdata/simstrat_aed2/simstrat.par",
                         simplifyVector = FALSE)
mp <- par[["ModelParameters"]]

phys_pars <- c("a_seiche", "a_seiche_w", "f_wind", "c10", "cd", "hgeo",
              "p_air", "p_sw_water", "p_lw", "p_windf", "p_absorb",
              "beta_sol", "wat_albedo", "q_nn")

phys_df <- data.frame(
  model = "simstrat_aed2",
  file = "simstrat.par",
  name = paste0("ModelParameters/", phys_pars),
  value = sapply(phys_pars, \(p) as.numeric(mp[[p]])),
  par = phys_pars,
  module = "hydrodynamic",
  row.names = NULL
)

# AED2 biogeochemical initial-concentration parameters from the aed2.nml
# template (the same set that initialise_aed2() can write to)
aed2_nml <- read_nml("inst/extdata/simstrat_aed2/aed2.nml")
aed2_pars <- list(
  aed2_oxygen      = "oxy_initial",
  aed2_carbon      = c("dic_initial", "ch4_initial"),
  aed2_silica      = "rsi_initial",
  aed2_nitrogen    = c("amm_initial", "nit_initial"),
  aed2_phosphorus  = "frp_initial",
  aed2_organic_matter = c("poc_initial", "doc_initial", "pon_initial",
                          "don_initial", "pop_initial", "dop_initial")
)

aed2_df <- do.call(rbind, lapply(names(aed2_pars), \(block) {
  pars <- aed2_pars[[block]]
  data.frame(
    model = "simstrat_aed2",
    file = "aed2.nml",
    name = paste0(block, "/", pars),
    value = sapply(pars, \(p) as.numeric(get_nml_value(aed2_nml, p))),
    par = pars,
    module = "bgc",
    row.names = NULL
  )
}))

param_colnames <- param_colnames()
simstrat_aed2_parameters <- dplyr::bind_rows(phys_df, aed2_df) |>
  dplyr::mutate(
    min = 0.5 * value,
    max = 1.5 * value,
    index = NA_integer_,
    group = NA_character_
  ) |>
  dplyr::select(dplyr::any_of(param_colnames)) |>
  tibble::as_tibble()

usethis::use_data(simstrat_aed2_parameters, overwrite = TRUE)
