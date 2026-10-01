#' AED module prefix -> module name map
#'
#' Shared between GLM-AED (\code{\link{initialise_aed}}) and Simstrat-AED
#' (\code{initialise_simstrat_aed}, once added) -- both link the same AED
#' library, so they must resolve active modules identically for the same
#' `model_controls`. See \code{\link{resolve_aed_active_modules}}.
#'
#' @keywords internal
#' @noRd
.aed_module_map <- c(NCS = "aed_noncohesive", OXY = "aed_oxygen",
                     SIL = "aed_silica", NIT = "aed_nitrogen",
                     PHS = "aed_phosphorus", OGM = "aed_organic_matter",
                     PHY = "aed_phytoplankton", ZOO = "aed_zooplankton")

#' Canonical AED module ordering
#'
#' Only real `&aed_models` `models` entries belong here -- this vector orders
#' the `models` list written to aed.nml. It is NOT the physical order of the
#' `&<block>` sections in the file. `aed_noncohesive` in particular must be
#' written *after* the `&aed_sed_const2d` block (libaed reads the module
#' namelists in one forward pass without rewinding, so an earlier
#' `&aed_noncohesive` is never found and GLM aborts); that block ordering is
#' fixed in the bundled `inst/extdata/aed/aed.nml` template and preserved by
#' `read_nml()`/`write_nml()`, not controlled here.
#' @keywords internal
#' @noRd
.aed_module_order <- c("aed_sedflux", "aed_noncohesive",
                       "aed_oxygen", "aed_silica", "aed_nitrogen",
                       "aed_phosphorus", "aed_organic_matter",
                       "aed_phytoplankton", "aed_zooplankton",
                       "aed_macrophyte", "aed_totals")

#' AED cross-module dependencies, keyed by dependent module
#'
#' Verified directly against this package's own bundled `aed.nml`
#' target-variable links (not just libaed-water/libaed-api's compiled-in
#' Fortran defaults, which for some parameters differ from what is actually
#' configured here -- e.g. `aed_phytoplankton`'s `c_uptake_target_variable`
#' is blank in this template, so no `aed_carbon` module is required at all,
#' unlike AED2/Simstrat's `aed2_phytoplankton`, which does link to
#' `aed2_carbon`; see `initialise_aed2()`):
#'  - aed_oxygen         -> aed_sedflux        (`fsed_oxy_variable = 'SDF_Fsed_oxy'`)
#'  - aed_silica         -> aed_oxygen         (`silica_reactant_variable = 'OXY_oxy'`)
#'  - aed_nitrogen       -> aed_oxygen,        (`nitrif_reactant_variable = 'OXY_oxy'`)
#'                          aed_sedflux        (`fsed_amm_variable`/`fsed_nit_variable`)
#'  - aed_phosphorus     -> aed_oxygen,        (`phosphorus_reactant_variable = 'OXY_oxy'`)
#'                          aed_sedflux        (`fsed_frp_variable`)
#'  - aed_organic_matter -> aed_oxygen,        (`dom_miner_oxy_reactant_var = 'OXY_oxy'`)
#'                          aed_nitrogen,      (`dom_miner_nit_reactant_var`/`don_miner_product_variable`)
#'                          aed_phosphorus     (`dop_miner_product_variable = 'PHS_frp'`)
#'  - aed_phytoplankton  -> aed_oxygen, aed_nitrogen, aed_phosphorus,
#'                          aed_silica, aed_organic_matter (uptake/
#'                          excretion/mortality target variables)
#'  - aed_zooplankton    -> aed_organic_matter (excretion/mortality target
#'                          variables)
#' `aed_noncohesive` (suspended-sediment groups `NCS_ss*`) runs standalone
#' in the bundled template -- `settling`/`resuspension` are self-contained
#' and it has no target-variable links back into other modules -- so it
#' carries no forced dependencies. It is force-included whenever any
#' `NCS_ss*` state variable is simulated (via the `NCS` prefix in
#' \code{.aed_module_map}) and is also referenced by `aed_totals`'
#' `TSS_vars`. Its `&aed_noncohesive` block must sit *after*
#' `&aed_sed_const2d` in aed.nml (see \code{.aed_module_order}); the bundled
#' template is ordered that way.
#' `aed_macrophyte` has no equivalent source file in the current
#' libaed-water/libaed-api checkouts (seemingly superseded/removed there)
#' and has no target-variable keys in the bundled template either, so its
#' dependencies can't be verified the same way -- left with no forced
#' dependencies, as before. `aed_totals` is force-included by callers
#' whenever NIT_tn/PHS_tp/CAR_toc is requested; its own TN_vars/TP_vars/
#' TOC_vars in the bundled template ('NIT_nit','NIT_amm','OGM_don','OGM_pon',
#' 'PHY_green_IN'; 'PHS_frp','OGM_dop','OGM_pop','PHY_green_IP';
#' 'OGM_doc','OGM_poc','PHY_green','PHY_diatom') reference aed_nitrogen,
#' aed_phosphorus, aed_organic_matter, and aed_phytoplankton -- so those
#' must be forced too, or GLM aborts with "Undefined variable" (same
#' failure mode as aed2_phytoplankton without its dependencies; see
#' `initialise_aed2()`).
#'
#' @keywords internal
#' @noRd
.aed_module_deps <- list(
  aed_oxygen         = "aed_sedflux",
  aed_silica         = "aed_oxygen",
  aed_nitrogen       = c("aed_oxygen", "aed_sedflux"),
  aed_phosphorus     = c("aed_oxygen", "aed_sedflux"),
  aed_organic_matter = c("aed_oxygen", "aed_nitrogen", "aed_phosphorus"),
  aed_phytoplankton  = c("aed_oxygen", "aed_nitrogen", "aed_phosphorus",
                         "aed_silica", "aed_organic_matter"),
  aed_zooplankton    = "aed_organic_matter",
  aed_totals         = c("aed_nitrogen", "aed_phosphorus",
                         "aed_organic_matter", "aed_phytoplankton")
)

#' Map AED variable-name prefixes to their owning (base) modules
#'
#' @param prefixes character vector; variable-name prefixes (e.g. from
#' `sub("_.*$", "", var_names)`) for the state variables being simulated.
#' @param module_map named character vector; prefix -> module name.
#'
#' @return character vector of base module names (unordered, deduplicated).
#' @keywords internal
#' @noRd
aed_prefixes_to_modules <- function(prefixes, module_map = .aed_module_map) {
  unique(unname(module_map[prefixes]))
}

#' Resolve the full set of active AED modules from a base module set
#'
#' Shared resolution engine for AED (v3) module activation: expands a base
#' set of active modules (e.g. from \code{\link{aed_prefixes_to_modules}} and/
#' or explicitly forced modules like `aed_totals`) to a fixed point over
#' `module_deps`, so every module a requested one depends on is included too
#' (repeated until no new modules are added). Used identically by GLM-AED and
#' Simstrat-AED (\code{\link{initialise_aed}} and `initialise_simstrat_aed()`)
#' so both couplings activate AED modules the same way for the same
#' `model_controls` -- see the module docs on \code{\link{.aed_module_deps}}
#' for why each dependency exists.
#'
#' @param active_modules character vector; base set of active module names.
#' @param module_order character vector; canonical module ordering, used to
#' order the returned set.
#' @param module_deps named list; module -> vector of modules it depends on.
#'
#' @return character vector of active module names, in `module_order` order.
#' @keywords internal
#' @noRd
resolve_aed_active_modules <- function(active_modules,
                                       module_order = .aed_module_order,
                                       module_deps = .aed_module_deps) {
  active_modules <- module_order[module_order %in% active_modules]

  repeat {
    added <- unlist(module_deps[active_modules], use.names = FALSE)
    new_active <- union(active_modules, added)
    if (setequal(new_active, active_modules)) break
    active_modules <- new_active
  }
  module_order[module_order %in% active_modules]
}
