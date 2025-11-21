#' Derived variable registry and dependency mapping
#'
#' These internal objects define the mapping from derived variable names to
#' the functions that compute them (`.deriv_registry`) and the dependency
#' relationships between variables (`.deriv_deps`). The dependency mapping is
#' used so that requesting a variable automatically triggers calculation of
#' any prerequisite variables.
#'
#' These registries are used internally by `add_deriv_output()` and are not
#' exported as part of the package’s public API.
#'
#' @format
#' * `.deriv_registry`: A named list where each element is a function of the
#'   form `fun(out_list, hyps)` that returns a derived variable vector.
#'
#' * `.deriv_deps`: A named list where each element is a character vector of
#'   variable names that must be calculated before the variable can be
#'   computed.
#'
#' @keywords internal
#'
#' @name deriv_registry
#' @seealso [add_deriv_output()], [resolve_dependencies()]
#' @noRd
NULL

# registry: variable -> computing function
.deriv_registry <- list(
  # HYD variables
  HYD_strat  = calc_HYD_strat,
  HYD_thmcln = calc_HYD_thmcln,
  HYD_ctrbuy = calc_HYD_ctrbuy,
  HYD_epidep = calc_HYD_epidep,
  HYD_hypdep = calc_HYD_hypdep,
  HYD_schstb = calc_HYD_schstb,
  # CHM oxygen variables
  CHM_oxyepi = calc_CHM_oxyepi,
  CHM_oxyhyp = calc_CHM_oxyhyp,
  CHM_oxycln = calc_CHM_oxycln,
  CHM_oxymet = calc_CHM_oxymet,
  CHM_oxymom = calc_CHM_oxymom,
  CHM_oxynal = calc_CHM_oxynal,
  # Lake trophic index variables
  LKE_tlic   = calc_LKE_tlic,
  LKE_tlin   = calc_LKE_tlin,
  LKE_tlip   = calc_LKE_tlip,
  LKE_tlise  = calc_LKE_tlise,
  LKE_tli3   = calc_LKE_tli3,
  LKE_tli4   = calc_LKE_tli4
)

.deriv_deps <- list(
  # HYD
  HYD_strat  = character(0),
  HYD_thmcln = character(0),
  HYD_ctrbuy = character(0),
  HYD_epidep = character(0),
  HYD_hypdep = character(0),
  HYD_schstb = character(0),
  
  # CHM oxygen
  CHM_oxyepi = "HYD_epidep",
  CHM_oxyhyp = "HYD_hypdep",
  CHM_oxycln = c("HYD_epidep", "HYD_hypdep"),
  CHM_oxymet = c("HYD_epidep", "HYD_hypdep"),
  CHM_oxymom = c("HYD_epidep", "HYD_hypdep"),
  CHM_oxynal = character(0),
  
  # Lake trophic index requires epilimnetic depth
  LKE_tlic   = "HYD_epidep",
  LKE_tlin   = "HYD_epidep",
  LKE_tlip   = "HYD_epidep",
  LKE_tlise = character(0),
  LKE_tli3   = "HYD_epidep",
  LKE_tli4   = "HYD_epidep"
)
