#' Map of GLM-AED diagnostic fluxes onto a C/N/P budget
#'
#' Each row ties a diagnostic variable written by libaed-water to the element
#' it carries, how it is integrated over the lake, and which two compartments
#' it connects. The compartments are the boxes of the budget diagram:
#' `inorganic` (DIN, FRP and adsorbed P, DIC), `phyto` (plankton), `dom`
#' (dissolved organic matter), `pom` (particulate organic matter) inside the
#' lake, and `sediment`, `atmosphere`, `inflow` and `outflow` outside it.
#' A flux is positive in the `from` to `to` direction. Interface terms that
#' AED reports as "into the water" (`*_dsf`, `*_res`) are therefore
#' `sediment` to `x`, and settling is negative.
#'
#' `kind` says how the variable is turned into mmol per day for the whole
#' lake: `vol` is a rate per m3 summed over the layer volumes, `zone` a rate
#' per m2 of sediment zone multiplied by the zone area, `surface` a rate per m2
#' multiplied by the water surface area, and `settle` a settling rate per m3
#' multiplied by the layer thickness and the area of sediment exposed in the
#' layer (the change in the hypsograph across it), which is the mass that
#' reaches the sediment.
#'
#' `method` is `"all"` for fluxes used whatever the `settling` option,
#' `"hypsograph"` for settling worked out from the cell settling rates, and
#' `"interface"` for the sediment-water interface diagnostics (`*_swi_Z`),
#' which AED scales with a layer thickness that is not that of the layer the
#' rate was calculated in and so are far larger than the mass that leaves the
#' water column (see [calc_glm_fluxes()]).
#'
#' The refractory and coarse pools (`OGM_docr`, `OGM_donr`, `OGM_dopr`,
#' `OGM_cpom`) are not part of the `aed_totals`, so are outside the budget.
#' Their mineralisation (`*_min` of `docr`, ...) feeds the labile DOM pool, not
#' the inorganic one, and is not mapped.
#'
#' Fluxes between two compartments that are both inside the budgeted pool are
#' internal and cancel in the closure. `inorganic` is not part of the carbon
#' pool (DIC is not a component of TOC), so for C the uptake and respiration
#' terms cross the pool boundary.
#'
#' @return a tibble: `pattern` (regular expression for the netCDF name),
#'   `element` (`"C"`, `"N"` or `"P"`), `kind`, `from`, `to`, `process`,
#'   `sign` (multiplier giving the `from` to `to` direction) and `method`.
#' @noRd
glm_flux_map <- function() {
  m <- function(pattern, element, kind, from, to, process, sign = 1,
                method = "all") {
    tibble::tibble(pattern = pattern, element = element, kind = kind,
                   from = from, to = to, process = process, sign = sign,
                   method = method)
  }
  dplyr::bind_rows(
    # ---- nitrogen ----
    m("^PHY_upt_(no3|nh4)$", "N", "vol", "inorganic", "phyto", "Uptake"),
    m("^PHY_upt_n2$", "N", "vol", "atmosphere", "phyto", "N2 fixation"),
    m("^PHY_.+_exc_n$", "N", "vol", "phyto", "dom", "Exudation"),
    m("^PHY_.+_mor_n$", "N", "vol", "phyto", "pom", "Mortality"),
    m("^PHY_.+_rsp_n$", "N", "vol", "phyto", "inorganic", "Excretion",
      sign = -1),
    m("^OGM_pon_hyd$", "N", "vol", "pom", "dom", "Hydrolysis"),
    m("^OGM_don_min$", "N", "vol", "dom", "inorganic", "Mineralisation"),
    m("^NIT_nitrif$", "N", "vol", "inorganic", "inorganic", "Nitrification"),
    m("^NIT_dnra$", "N", "vol", "inorganic", "inorganic", "DNRA"),
    m("^(NIT_denit|OGM_doc_denit)$", "N", "vol", "inorganic", "atmosphere",
      "Denitrification"),
    m("^NIT_anammox$", "N", "vol", "inorganic", "atmosphere", "Anammox"),
    m("^NIT_(amm|nit)_dsf_Z$", "N", "zone", "sediment", "inorganic",
      "Sediment release"),
    m("^OGM_don_swi_Z$", "N", "zone", "sediment", "dom", "Sediment DON"),
    m("^OGM_pon_res_Z$", "N", "zone", "sediment", "pom", "Resuspension"),
    m("^OGM_pon_set$", "N", "settle", "sediment", "pom", "Settling",
      method = "hypsograph"),
    m("^PHY_.+_set_n$", "N", "settle", "sediment", "phyto",
      "Phytoplankton settling", method = "hypsograph"),
    m("^OGM_pon_swi_Z$", "N", "zone", "sediment", "pom",
      "Resuspension - settling", method = "interface"),
    m("^PHY_phy_swi_n_Z$", "N", "zone", "sediment", "phyto",
      "Phytoplankton settling", method = "interface"),
    m("^NIT_din_atm$", "N", "surface", "atmosphere", "inorganic",
      "Deposition"),
    # ---- phosphorus ----
    m("^PHY_upt_po4$", "P", "vol", "inorganic", "phyto", "Uptake"),
    m("^PHY_.+_exc_p$", "P", "vol", "phyto", "dom", "Exudation"),
    m("^PHY_.+_mor_p$", "P", "vol", "phyto", "pom", "Mortality"),
    m("^PHY_.+_rsp_p$", "P", "vol", "phyto", "inorganic", "Excretion",
      sign = -1),
    m("^OGM_pop_hyd$", "P", "vol", "pom", "dom", "Hydrolysis"),
    m("^OGM_dop_min$", "P", "vol", "dom", "inorganic", "Mineralisation"),
    m("^PHS_frp_dsf_Z$", "P", "zone", "sediment", "inorganic",
      "Sediment release"),
    m("^OGM_dop_swi_Z$", "P", "zone", "sediment", "dom", "Sediment DOP"),
    m("^OGM_pop_res_Z$", "P", "zone", "sediment", "pom", "Resuspension"),
    m("^OGM_pop_set$", "P", "settle", "sediment", "pom", "Settling",
      method = "hypsograph"),
    m("^PHY_.+_set_p$", "P", "settle", "sediment", "phyto",
      "Phytoplankton settling", method = "hypsograph"),
    m("^PHS_frp_ads_res_Z$", "P", "zone", "sediment", "inorganic",
      "Adsorbed P resuspension"),
    m("^PHS_frp_ads_set$", "P", "settle", "sediment", "inorganic",
      "Adsorbed P settling", method = "hypsograph"),
    m("^OGM_pop_swi_Z$", "P", "zone", "sediment", "pom",
      "Resuspension - settling", method = "interface"),
    m("^PHY_phy_swi_p_Z$", "P", "zone", "sediment", "phyto",
      "Phytoplankton settling", method = "interface"),
    m("^PHS_frp_ads_swi_Z$", "P", "zone", "sediment", "inorganic",
      "Adsorbed P exchange", method = "interface"),
    m("^PHS_dip_atm$", "P", "surface", "atmosphere", "inorganic",
      "Deposition"),
    # ---- carbon ----
    m("^PHY_upt_dic$", "C", "vol", "inorganic", "phyto", "Photosynthesis"),
    m("^PHY_.+_rsp_c$", "C", "vol", "phyto", "inorganic", "Respiration",
      sign = -1),
    m("^PHY_.+_exc_c$", "C", "vol", "phyto", "dom", "Exudation"),
    m("^PHY_.+_mor_c$", "C", "vol", "phyto", "pom", "Mortality"),
    m("^OGM_poc_hyd$", "C", "vol", "pom", "dom", "Hydrolysis"),
    m("^OGM_doc_min$", "C", "vol", "dom", "inorganic", "Mineralisation"),
    m("^OGM_photolysis$", "C", "vol", "dom", "inorganic", "Photolysis"),
    m("^OGM_doc_floc$", "C", "vol", "dom", "pom", "Flocculation"),
    m("^OGM_doc_swi_Z$", "C", "zone", "sediment", "dom", "Sediment DOC"),
    m("^OGM_poc_res_Z$", "C", "zone", "sediment", "pom", "Resuspension"),
    m("^OGM_(poc|cpom)_set$", "C", "settle", "sediment", "pom",
      "Settling", method = "hypsograph"),
    m("^PHY_.+_set_c$", "C", "settle", "sediment", "phyto",
      "Phytoplankton settling", method = "hypsograph"),
    m("^OGM_poc_swi_Z$", "C", "zone", "sediment", "pom",
      "Resuspension - settling", method = "interface"),
    m("^PHY_phy_swi_c_Z$", "C", "zone", "sediment", "phyto",
      "Phytoplankton settling", method = "interface"),
    m("^CAR_sed_dic_Z$", "C", "zone", "sediment", "inorganic", "Sediment DIC"),
    m("^CAR_atm_co2_flux$", "C", "surface", "atmosphere", "inorganic",
      "CO2 exchange")
  )
}

#' Compartments inside the budgeted pool of each element
#'
#' @param element `"C"`, `"N"` or `"P"`.
#' @return character vector of compartment names.
#' @noRd
glm_budget_pool <- function(element) {
  if (element == "C") c("phyto", "dom", "pom")
  else c("inorganic", "phyto", "dom", "pom")
}

#' Compartment (budget box) of a GLM-AED state variable
#'
#' @param var character; GLM-AED state variable names.
#' @return character vector of compartments, `NA` where not recognised.
#' @noRd
glm_var_compartment <- function(var) {
  dplyr::case_when(
    grepl("^(NIT_|SIL_)", var) ~ "inorganic",
    grepl("^PHS_", var) ~ "inorganic",
    grepl("^CAR_(dic|ch4)", var) ~ "inorganic",
    grepl("^OGM_d", var) ~ "dom",
    grepl("^OGM_(p|c)", var) ~ "pom",
    grepl("^(PHY|ZOO)_", var) ~ "phyto",
    .default = NA_character_
  )
}

#' Molar mass of the budget elements (g/mol)
#' @noRd
glm_element_mass <- function() c(C = 12.011, N = 14.007, P = 30.974)

#' Names of the budget elements
#' @noRd
glm_element_names <- function() {
  c(C = "Carbon", N = "Nitrogen", P = "Phosphorus")
}
