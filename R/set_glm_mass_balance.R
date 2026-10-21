#' Populate the GLMv4 `&mass_balance` block from the active AED variables
#'
#' GLMv4 added a `&mass_balance` namelist block that writes a water/mass
#' balance diagnostic CSV for a chosen set of water-quality variables. GLM
#' accepts at most 15 `balance_vars`, so AEME by default reports only the
#' components of the nitrogen (`TN`), phosphorus (`TP`) and suspended solids
#' (`TSS`) totals, derived from the `aed_totals` logic in
#' `derive_aed_totals()`. Use `groups` to choose other groups.
#'
#' The groups are:
#' * `"TN"`, `"TP"`, `"TOC"`, `"TSS"`: the components summed into each
#'   `aed_totals` variable (module-filtered to the active AED modules);
#' * `"other"`: the remaining active state variables from the
#'   `&init_profiles` `wq_names` (e.g. oxygen, silica).
#'
#' Variables are added in the order of `groups`; if more than 15 result, the
#' list is truncated with a warning. When no biogeochemistry is active, or no
#' variable qualifies, it falls back to `balance_varnum = 0` and drops
#' `balance_vars`.
#'
#' Only touches `glm_nml` when it already carries a `&mass_balance` block
#' (i.e. the hydrodynamic template is a glm4.nml); older GLM builds have no
#' such block and are returned unchanged.
#'
#' @param glm_nml list; parsed GLM nml object, after `initialise_glm()`.
#' @param use_bgc logical; is the biogeochemistry (AED) library active?
#' @param path_glm directory holding the GLM nml and `aed/` configuration,
#'   used to derive the totals' components.
#' @param groups character; groups to report (see above). Defaults to the
#'   option `AEME.glm_balance_groups`, or `c("TN", "TP", "TSS")`.
#'
#' @return the updated `glm_nml` list.
#' @noRd
set_glm_mass_balance <- function(glm_nml, use_bgc = TRUE, path_glm = NULL,
                                 groups = getOption("AEME.glm_balance_groups",
                                                    .glm_balance_groups_default)) {

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
    wq_names <- select_glm_balance_vars(wq_names, path_glm, groups)
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

# GLM errors if more than this many balance_vars are given
.glm_max_balance_vars <- 15L
.glm_balance_groups_default <- c("TN", "TP", "TSS")
.glm_balance_groups_all <- c("TN", "TP", "TOC", "TSS", "other")

#' Choose the mass balance variables for the requested groups
#'
#' @param wq_names character; active state variables (`&init_profiles`).
#' @param path_glm GLM-AED model directory (for the totals' components).
#' @param groups character; see `set_glm_mass_balance()`.
#' @return character vector of at most 15 variable names.
#' @noRd
select_glm_balance_vars <- function(wq_names, path_glm, groups) {
  groups <- unique(as.character(groups))
  bad <- setdiff(groups, .glm_balance_groups_all)
  if (length(bad) > 0) {
    valid <- .glm_balance_groups_all
    cli::cli_abort(c("Unknown mass balance group{?s} {.val {bad}}.",
                     i = "Choose from {.val {valid}}."))
  }
  comps <- glm_totals_component_vars(path_glm)
  if (is.null(comps)) {
    # totals cannot be derived: fall back to the active states
    vars <- wq_names
  } else {
    vars <- unlist(lapply(groups, function(g) {
      if (g == "other") setdiff(wq_names, unlist(comps)) else comps[[g]]
    }), use.names = FALSE)
  }
  vars <- unique(vars)
  max_n <- .glm_max_balance_vars
  if (length(vars) > max_n) {
    n_all <- length(vars)
    cli::cli_warn("GLM supports at most {max_n} mass balance variables;
                  keeping the first {max_n} of {n_all}. Choose fewer
                  {.arg groups}.")
    vars <- vars[seq_len(max_n)]
  }
  vars
}

#' Choose the GLM-AED mass balance variable groups
#'
#' GLM accepts at most 15 `balance_vars` in the `&mass_balance` block, so
#' AEME reports only the nitrogen, phosphorus and suspended-solids totals'
#' components by default. This sets which groups are reported for an existing
#' configuration, based on the AED configuration (the `aed_totals` components
#' for the phytoplankton groups and AED modules that are active).
#'
#' The default for new builds can be changed with
#' `options(AEME.glm_balance_groups = c("TN", "TP", "TOC"))`.
#'
#' @inheritParams build_aeme
#' @param groups character; any of `"TN"`, `"TP"`, `"TOC"` (components of the
#'   total nitrogen, phosphorus and organic carbon), `"TSS"` (suspended solids)
#'   and `"other"` (remaining active state variables such as oxygen and
#'   silica). Variables are taken in this order and truncated at 15 with a
#'   warning.
#'
#' @return `aeme` with the `&mass_balance` block of the `glm_aed` configuration
#'   updated. Use [write_configuration()] to write it to file.
#' @export
#' @examples
#' \dontrun{
#' aeme <- set_glm_balance_groups(aeme, groups = c("TN", "TOC"))
#' }
set_glm_balance_groups <- function(aeme, groups = c("TN", "TP", "TSS"),
                                   path = NULL) {
  aeme <- check_aeme(aeme)
  cfg <- configuration(aeme)
  hd <- cfg[["glm_aed"]][["hydrodynamic"]]
  if (is.null(hd)) {
    cli::cli_abort(c("No hydrodynamic configuration for {.val glm_aed}.",
                     i = "Run {.fn build_aeme} first."))
  }
  if (is.null(hd[["mass_balance"]])) {
    cli::cli_abort("The GLM configuration has no {.field &mass_balance} block
                   (GLMv4 only).")
  }
  path_glm <- file.path(get_lake_dir(aeme, path), "glm_aed")
  hd <- set_glm_mass_balance(hd, use_bgc = !is.null(hd[["wq_setup"]]),
                             path_glm = path_glm, groups = groups)
  cfg[["glm_aed"]][["hydrodynamic"]] <- hd
  configuration(aeme) <- cfg
  aeme
}
