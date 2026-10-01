#' Populate the GLMv4 `&mass_balance` block from the active AED variables
#'
#' GLMv4 added a `&mass_balance` namelist block that writes a water/mass
#' balance diagnostic CSV for a chosen set of water-quality variables. AEME
#' fills `balance_vars` with the GLM-AED state variables that are switched on
#' -- taken straight from the `&init_profiles` `wq_names` that
#' `initialise_glm()` has already written, so the two lists cannot drift
#' apart (in particular both exclude the totals / particulate-inorganic /
#' `NCS_ss*` variables that GLM cannot find; see `glm_non_state_vars()`).
#' When no biogeochemistry is active, or no variable qualifies, it falls
#' back to `balance_varnum = 0` and drops `balance_vars`.
#'
#' Only touches `glm_nml` when it already carries a `&mass_balance` block
#' (i.e. the hydrodynamic template is a glm4.nml); older GLM builds have no
#' such block and are returned unchanged.
#'
#' @param glm_nml list; parsed GLM nml object, after `initialise_glm()`.
#' @param use_bgc logical; is the biogeochemistry (AED) library active?
#'
#' @return the updated `glm_nml` list.
#' @noRd
set_glm_mass_balance <- function(glm_nml, use_bgc = TRUE) {

  mb <- glm_nml[["mass_balance"]]
  if (is.null(mb)) {
    return(glm_nml)
  }

  wq_names <- character(0)
  if (isTRUE(use_bgc)) {
    wq_names <- glm_nml[["init_profiles"]][["wq_names"]]
    # wq_names may be a character vector, a single comma-joined string, or
    # the "''" placeholder initialise_glm() writes when there are none.
    wq_names <- unlist(strsplit(as.character(wq_names), "\\s*,\\s*"))
    wq_names <- trimws(gsub("'", "", wq_names))
    wq_names <- wq_names[nzchar(wq_names)]
  }

  mb[["balance_file"]] <- mb[["balance_file"]] %||% "mass_balance"
  if (length(wq_names) > 0) {
    mb[["balance_varnum"]] <- length(wq_names)
    mb[["balance_vars"]] <- wq_names
  } else {
    mb[["balance_varnum"]] <- 0
    mb[["balance_vars"]] <- NULL
  }
  mb[["timezone"]] <- mb[["timezone"]] %||% 0

  glm_nml[["mass_balance"]] <- mb
  glm_nml
}
