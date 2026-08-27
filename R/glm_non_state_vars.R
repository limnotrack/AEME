#' AEME variables that are not GLM-AED water-column state variables
#'
#' The variables switched on in `model_controls` include several that are
#' *not* GLM-AED state variables and therefore must never be listed in the
#' `&init_profiles` `wq_names` (or the GLMv4 `&mass_balance` `balance_vars`)
#' block -- GLM aborts with `Cannot find "<var>" for initial value` /
#' `... for mass balance output` if they appear there:
#'
#' * aggregate totals `NIT_tn` / `PHS_tp` / `CAR_toc` -> AED diagnostics
#'   `TOT_tn` / `TOT_tp` / `TOT_toc`, computed by `aed_totals`;
#' * particulate-inorganic pools `PHS_pip` (-> `PHS_frp_ads`, only a real
#'   state variable when `simPO4Adsorption` is on) and `NIT_pin`;
#' * total chlorophyll `PHY_tchla` (a phytoplankton diagnostic);
#' * non-cohesive sediment groups `NCS_ss*` (initialised inside
#'   `aed_noncohesive` via `ss_initial`, not through GLM);
#' * the physical / forcing columns that never carry an initial
#'   concentration.
#'
#' This mirrors the exclusion list `initialise_aed()` already applies when
#' writing the AED nml, plus the `NCS_ss*` groups, so GLM's initial-profile
#' and mass-balance variable lists stay in step with what AED actually
#' registers.
#'
#' @return character vector of `var_aeme` names to exclude.
#' @noRd
glm_non_state_vars <- function() {
  env <- new.env(parent = emptyenv())
  data("key_naming", package = "AEME", envir = env)
  deriv_vars <- env$key_naming[["var_aeme"]][env$key_naming[["derived"]] %in% TRUE]

  c(
    deriv_vars,
    "DateTime", "HYD_flow", "HYD_temp", "HYD_dens", "LKE_lvlwtr",
    "RAD_par", "RAD_extc", "RAD_secchi", "CHM_salt",
    "PHS_pip", "NIT_pin", "PHS_tp", "NIT_tn", "PHY_tchla", "CAR_toc",
    paste0("NCS_ss", 1:6), "NCS_iss", "NCS_tss"
  ) |>
    unique()
}
