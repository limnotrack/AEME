#' Populate the GLMv4 `&mass_balance` block from the active AED variables
#'
#' GLMv4 added a `&mass_balance` namelist block that writes a water/mass
#' balance diagnostic CSV for a chosen set of water-quality variables. By
#' default AEME reports every component of the `aed_totals` (nitrogen `TN`,
#' phosphorus `TP`, organic carbon `TOC` and suspended solids `TSS`), derived
#' from the `aed_totals` logic in `derive_aed_totals()`. Use `groups` to choose
#' other groups, and [add_glm_balance_vars()] to add groups or individual
#' variables to an existing configuration.
#'
#' The groups are:
#' * `"TN"`, `"TP"`, `"TOC"`, `"TSS"`: the components summed into each
#'   `aed_totals` variable (module-filtered to the active AED modules);
#' * `"other"`: the remaining active state variables from the
#'   `&init_profiles` `wq_names` (e.g. oxygen, silica).
#'
#' Variables are added in the order of `groups`. When no biogeochemistry is
#' active, or no variable qualifies, it falls back to `balance_varnum = 0` and
#' drops `balance_vars`. There is no limit on the number of variables here,
#' but GLM builds before 4.0.0+ accept at most 15 `balance_vars` and stop with
#' an error if more are given.
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
#'   option `AEME.glm_balance_groups`, or `c("TN", "TP", "TOC", "TSS")`.
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
    wq_names <- glm_active_wq_names(glm_nml)
    wq_names <- select_glm_balance_vars(wq_names, path_glm, groups)
  }

  glm_nml[["mass_balance"]] <- glm_mass_balance_block(mb, wq_names)
  glm_nml
}

.glm_balance_groups_default <- c("TN", "TP", "TOC", "TSS")
.glm_balance_groups_all <- c("TN", "TP", "TOC", "TSS", "other")

#' Set the variables of a `&mass_balance` block
#'
#' @param mb list; the `mass_balance` block.
#' @param vars character; balance variables, empty for none.
#' @return the updated block.
#' @noRd
glm_mass_balance_block <- function(mb, vars) {
  mb[["balance_file"]] <- mb[["balance_file"]] %||% "mass_balance"
  if (length(vars) > 0) {
    mb[["balance_varnum"]] <- length(vars)
    mb[["balance_vars"]] <- vars
  } else {
    mb[["balance_varnum"]] <- 0
    mb[["balance_vars"]] <- NULL
  }
  mb[["timezone"]] <- mb[["timezone"]] %||% 0
  mb
}

#' Active state variables listed in `&init_profiles`
#'
#' `wq_names` may be a character vector, a single comma-joined string, or the
#' `"''"` placeholder `initialise_glm()` writes when there are none.
#'
#' @param glm_nml list; parsed GLM nml object.
#' @return character vector of variable names.
#' @noRd
glm_active_wq_names <- function(glm_nml) {
  wq_names <- glm_nml[["init_profiles"]][["wq_names"]]
  wq_names <- unlist(strsplit(as.character(wq_names), "\\s*,\\s*"))
  wq_names <- trimws(gsub("'", "", wq_names))
  wq_names[nzchar(wq_names)]
}

#' Choose the mass balance variables for the requested groups
#'
#' @param wq_names character; active state variables (`&init_profiles`).
#' @param path_glm GLM-AED model directory (for the totals' components).
#' @param groups character; see `set_glm_mass_balance()`.
#' @return character vector of variable names.
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
  unique(vars)
}

#' Choose the GLM-AED mass balance variable groups
#'
#' Sets which variables the GLM `&mass_balance` block reports for an existing
#' configuration, based on the AED configuration (the `aed_totals` components
#' for the phytoplankton groups and AED modules that are active). By default
#' all components of the nitrogen, phosphorus, organic carbon and suspended
#' solids totals are reported. This replaces the current list; use
#' [add_glm_balance_vars()] to add to it.
#'
#' The default for new builds can be changed with
#' `options(AEME.glm_balance_groups = c("TN", "TP"))`.
#'
#' GLM builds before 4.0.0+ accept at most 15 balance variables and stop with
#' an error if more are given; with such a build choose fewer `groups`.
#'
#' @inheritParams build_aeme
#' @param groups character; any of `"TN"`, `"TP"`, `"TOC"` (components of the
#'   total nitrogen, phosphorus and organic carbon), `"TSS"` (suspended solids)
#'   and `"other"` (remaining active state variables such as oxygen and
#'   silica). Variables are taken in this order.
#'
#' @return `aeme` with the `&mass_balance` block of the `glm_aed` configuration
#'   updated. Use [write_configuration()] to write it to file.
#' @seealso [add_glm_balance_vars()]
#' @export
#' @examples
#' \dontrun{
#' aeme <- set_glm_balance_groups(aeme, groups = c("TN", "TOC"))
#' }
set_glm_balance_groups <- function(aeme, groups = .glm_balance_groups_default,
                                   path = NULL) {
  aeme <- check_aeme(aeme)
  hd <- glm_hydrodynamic_mb(aeme)
  path_glm <- file.path(get_lake_dir(aeme, path), "glm_aed")
  hd <- set_glm_mass_balance(hd, use_bgc = !is.null(hd[["wq_setup"]]),
                             path_glm = path_glm, groups = groups)
  glm_set_hydrodynamic(aeme, hd)
}

#' Add variables to the GLM-AED mass balance
#'
#' Adds a group of variables, or individual state variables, to the
#' `&mass_balance` block of an existing `glm_aed` configuration, keeping the
#' variables already there. Variables already in the list are not repeated.
#'
#' @inheritParams set_glm_balance_groups
#' @param groups character; groups to add, any of `"TN"`, `"TP"`, `"TOC"`,
#'   `"TSS"` and `"other"`, as in [set_glm_balance_groups()]. `NULL` adds no
#'   group.
#' @param vars character; individual AED state variables to add, such as
#'   `"OXY_oxy"` or `"PHY_cyano"`. They must be listed in the `wq_names` of
#'   the configuration.
#'
#' @return `aeme` with the `&mass_balance` block of the `glm_aed` configuration
#'   updated. Use [write_configuration()] to write it to file.
#' @seealso [set_glm_balance_groups()]
#' @export
#' @examples
#' \dontrun{
#' aeme <- add_glm_balance_vars(aeme, groups = "other")
#' aeme <- add_glm_balance_vars(aeme, vars = c("OXY_oxy", "SIL_rsi"))
#' }
add_glm_balance_vars <- function(aeme, groups = NULL, vars = NULL,
                                 path = NULL) {
  aeme <- check_aeme(aeme)
  if (is.null(groups) && is.null(vars)) {
    cli::cli_abort("Provide {.arg groups} or {.arg vars} to add.")
  }
  hd <- glm_hydrodynamic_mb(aeme)
  wq_names <- glm_active_wq_names(hd)

  vars <- unique(as.character(vars))
  unknown <- setdiff(vars, wq_names)
  if (length(unknown) > 0) {
    cli::cli_abort(c("{.arg vars} {?is/are} not an active variable in the
                     configuration: {.val {unknown}}.",
                     i = "Active variables are listed in {.field wq_names}."))
  }
  new <- vars
  if (!is.null(groups)) {
    path_glm <- file.path(get_lake_dir(aeme, path), "glm_aed")
    new <- c(select_glm_balance_vars(wq_names, path_glm, groups), new)
  }

  hd[["mass_balance"]] <- glm_mass_balance_block(
    hd[["mass_balance"]],
    unique(c(as.character(hd[["mass_balance"]][["balance_vars"]]), new)))
  glm_set_hydrodynamic(aeme, hd)
}

#' GLM hydrodynamic configuration that carries a `&mass_balance` block
#'
#' @param aeme Aeme object.
#' @return the hydrodynamic configuration list.
#' @noRd
glm_hydrodynamic_mb <- function(aeme) {
  hd <- configuration(aeme)[["glm_aed"]][["hydrodynamic"]]
  if (is.null(hd)) {
    cli::cli_abort(c("No hydrodynamic configuration for {.val glm_aed}.",
                     i = "Run {.fn build_aeme} first."))
  }
  if (is.null(hd[["mass_balance"]])) {
    cli::cli_abort("The GLM configuration has no {.field &mass_balance} block
                   (GLMv4 only).")
  }
  hd
}

#' Store a GLM hydrodynamic configuration in an Aeme object
#' @noRd
glm_set_hydrodynamic <- function(aeme, hd) {
  cfg <- configuration(aeme)
  cfg[["glm_aed"]][["hydrodynamic"]] <- hd
  configuration(aeme) <- cfg
  aeme
}
