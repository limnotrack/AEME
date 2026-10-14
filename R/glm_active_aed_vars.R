#' AED module -> GLM-AED variable prefix map
#'
#' Variable-name prefix (as written by `rename_modelvars(type_output =
#' "glm_aed")`) owned by each AED module in `&aed_models` `models`. Unlike
#' `.aed_module_map` (module *activation*, from the variables requested) this
#' is used for the reverse: deciding which variables may be written to the GLM
#' nml given the modules that are actually switched on. It therefore also
#' covers `aed_carbon` (`CAR_dic`, `CAR_pH`, `CAR_ch4`).
#' @noRd
.glm_aed_prefix_map <- with(.aed_modules[!is.na(.aed_modules$prefix), ],
                                stats::setNames(prefix, module))

#' Active AED modules listed in an aed.nml
#'
#' @param aed_nml list; parsed aed.nml, or a path to one.
#' @return character vector of module names from `&aed_models` `models`,
#'   or `NULL` if the block is missing.
#' @noRd
glm_active_aed_models <- function(aed_nml) {
  if (is.character(aed_nml) && length(aed_nml) == 1 && file.exists(aed_nml)) {
    aed_nml <- read_nml(aed_nml)
  }
  models <- aed_nml[["aed_models"]][["models"]]
  if (is.null(models)) return(NULL)
  models <- unlist(strsplit(as.character(models), "\\s*,\\s*"))
  models <- trimws(gsub("['\"]", "", models))
  models[nzchar(models)]
}

#' Is a GLM-AED variable available given the active AED modules?
#'
#' A variable is kept when it does not belong to an AED module at all (`flow`,
#' `temp`, `salt`, `time`, ...) or when its owning module is in `aed_models`.
#'
#' @param glm_names character; GLM-AED variable names (e.g. `"CAR_pH"`).
#' @param aed_models character vector of active modules, or `NULL` for no
#'   filtering.
#' @return logical vector, `TRUE` where the variable should be kept.
#' @noRd
glm_aed_var_active <- function(glm_names, aed_models) {
  if (is.null(aed_models)) return(rep(TRUE, length(glm_names)))
  prefix <- sub("_.*$", "", glm_names)
  owner <- names(.glm_aed_prefix_map)[match(prefix, .glm_aed_prefix_map)]
  is.na(owner) | owner %in% aed_models
}

#' Initial water-quality profile arguments for `&init_profiles`
#'
#' @param model_controls data.frame of model controls.
#' @param n_depths integer; number of initial-profile depths.
#' @param aed_models character vector of active AED modules, or `NULL` to
#'   apply no module filter.
#' @return list with `wq_names`, `num_wq_vars`, `wq_init_vals`; `NULL` when
#'   `model_controls` has no candidate variables at all.
#' @noRd
glm_wq_init_args <- function(model_controls, n_depths, aed_models = NULL) {
  sim_vars <- model_controls |>
    dplyr::filter(simulate, !is.na(initial_wc),
                  !var_aeme %in% glm_non_state_vars())
  if (nrow(sim_vars) == 0) return(NULL)

  sim_vars <- sim_vars |>
    dplyr::mutate(glm_name = rename_modelvars(var_aeme,
                                              type_output = "glm_aed",
                                              passthrough = TRUE),
                  value = initial_wc * conversion_aed) |>
    dplyr::filter(!is.na(glm_name), nzchar(glm_name)) |>
    dplyr::distinct(var_aeme, .keep_all = TRUE)

  dropped <- sim_vars$glm_name[!glm_aed_var_active(sim_vars$glm_name,
                                                   aed_models)]
  if (length(dropped) > 0) {
    cli_inform_safe(c("i" = "Skipping GLM initial conditions for variables \\
                            whose AED module is not active: \\
                            {paste(dropped, collapse = ', ')}."))
  }
  sim_vars <- sim_vars[glm_aed_var_active(sim_vars$glm_name, aed_models), ]

  if (nrow(sim_vars) == 0) {
    return(list(wq_names = "''", num_wq_vars = 0, wq_init_vals = 0))
  }
  list(wq_names = sim_vars$glm_name,
       num_wq_vars = nrow(sim_vars),
       wq_init_vals = rep(sim_vars$value, each = n_depths))
}

#' Re-sync the AED-dependent GLM nml entries to the active AED modules
#'
#' Rebuilds `&init_profiles` water-quality variables (from `model_controls`),
#' the `&mass_balance` `balance_vars` (from those) and the `&inflow`
#' `inflow_vars` (from the inflow CSV headers) so none reference a variable
#' whose AED module is not in `aed_models`. Used when the AED modules are
#' changed after the model has been built, so it works from the sources (not
#' the already-filtered nml), letting modules be re-enabled later.
#'
#' @param glm_nml list; parsed GLM hydrodynamic nml.
#' @param model_controls data.frame of model controls.
#' @param aed_models character vector of active AED modules.
#' @param path_glm directory holding the GLM nml (inflow files are relative
#'   to it).
#' @return updated `glm_nml`.
#' @noRd
sync_glm_aed_vars <- function(glm_nml, model_controls, aed_models, path_glm) {
  # initial conditions
  ip <- glm_nml[["init_profiles"]]
  if (!is.null(ip) && !is.null(model_controls)) {
    n_depths <- ip[["num_depths"]] %||% length(ip[["the_depths"]])
    wq <- glm_wq_init_args(model_controls, n_depths, aed_models)
    if (is.null(wq)) {
      wq <- list(wq_names = "''", num_wq_vars = 0, wq_init_vals = 0)
    }
    for (nm in names(wq)) glm_nml[["init_profiles"]][[nm]] <- wq[[nm]]
  }

  # inflows
  inf <- glm_nml[["inflow"]]
  files <- inf[["inflow_fl"]]
  files <- files[!is.na(files) & nzchar(files)]
  if (length(files) > 0) {
    hdr <- names(utils::read.csv(file.path(path_glm, files[1]), nrows = 1,
                                 check.names = FALSE))
    vars <- hdr[-1]
    vars <- vars[glm_aed_var_active(vars, aed_models)]
    glm_nml[["inflow"]][["inflow_varnum"]] <- length(vars)
    glm_nml[["inflow"]][["inflow_vars"]] <- vars
  }

  # mass balance (mirrors init_profiles)
  set_glm_mass_balance(glm_nml, use_bgc = TRUE)
}
