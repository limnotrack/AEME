#' Write initial concentrations and active modules for a Simstrat-AED simulation
#'
#' @param model_controls dataframe of loaded model controls.
#' @param path_aed filepath; to the directory containing `aed.nml` (the
#' Simstrat-AED directory).
#' @param max_depth numeric; lake depth (m, positive), used to write a
#' top/bottom pair of rows to each `AED_initcond/<var>_ini.dat` file.
#' @param date_range length-2 Date/POSIXct vector; simulation start/stop,
#' used to write placeholder `AED_inflow/<var>_inflow.dat` files spanning
#' the whole simulation for any active state variable AEME does not
#' otherwise provide inflow data for.
#' @param ref_year integer; Simstrat `Simulation.Reference year`.
#'
#' @details Mirrors \code{\link{initialise_aed2}} (Simstrat-AED2's
#' equivalent), with two substantive differences instead of independent
#' logic: active-module detection is delegated to the same shared engine
#' (\code{\link{aed_prefixes_to_modules}}/\code{\link{resolve_aed_active_modules}})
#' that \code{\link{initialise_aed}} (GLM-AED) uses, and variable/file naming
#' goes through the `glm_aed`-identical `simstrat_aed` key-naming column --
#' both so `glm_aed` and `simstrat_aed` stay identical in their AED setup by
#' construction (same library, same module graph), rather than as two
#' independently-maintained copies. File-writing mechanics (placeholder
#' inflow files, `%`-syntax phyto/zoo par file handling) are otherwise the
#' same as `initialise_aed2()` -- see its `@details` for why those are
#' structured the way they are.
#'
#' @return Written `aed.nml`, `AED_initcond/<var>_ini.dat`, and
#' `AED_inflow/<var>_inflow.dat` files.
#' @noRd
#'
#' @importFrom dplyr filter pull
#' @importFrom readr read_csv
initialise_simstrat_aed <- function(model_controls, path_aed, max_depth = 10,
                                    date_range, ref_year) {

  aed_nml_file <- file.path(path_aed, "aed.nml")
  aed_nml <- read_nml(aed_nml_file)

  for (grp in c("aed_phytoplankton", "aed_zooplankton", "aed_macrophyte")) {
    dbase <- aed_nml[[grp]][["dbase"]]
    if (!is.null(dbase)) {
      aed_nml[[grp]][["dbase"]] <- basename(dbase)
    }
  }

  data("key_naming", package = "AEME", envir = environment())
  deriv_vars <- key_naming |>
    dplyr::filter(derived) |>
    dplyr::pull(var_aeme)

  this_ctrls <- model_controls |>
    dplyr::filter(simulate,
                  !var_aeme %in% deriv_vars,
                  !var_aeme %in% c("DateTime",
                                   "HYD_flow", "HYD_temp", "HYD_dens",
                                   "LKE_lvlwtr",
                                   "RAD_par", "RAD_extc", "RAD_secchi",
                                   "CHM_salt",
                                   "PHS_pip", "NIT_pin",
                                   "PHS_tp", "NIT_tn", "PHY_tchla", "CAR_toc")
    )

  if (nrow(this_ctrls) == 0) {
    cli_inform_safe(c("i" = "No variables to initialise in AED"))
    write_nml(aed_nml, aed_nml_file)
    return(invisible())
  }
  simstrat_names <- rename_modelvars(input = this_ctrls$var_aeme,
                                     type_output = "simstrat_aed",
                                     warn_unmatched = TRUE)
  keep <- !is.na(simstrat_names) & simstrat_names != ""
  this_ctrls <- this_ctrls[keep, ]
  simstrat_names <- simstrat_names[keep]

  # --- Determine active AED modules -------------------------------------
  # Same resolution engine as initialise_aed() (GLM-AED) -- see
  # resolve_aed_active_modules() for why this must be shared rather than a
  # second, independently-maintained copy of AED's module/dependency graph.
  prefixes <- sub("_.*$", "", simstrat_names)
  active_modules <- aed_prefixes_to_modules(prefixes)

  totals_vars <- c("NIT_tn", "PHS_tp", "CAR_toc")
  wants_totals <- any(model_controls$var_aeme %in% totals_vars &
                      model_controls$simulate)
  if (wants_totals) {
    active_modules <- union(active_modules, "aed_totals")
  }

  active_modules <- resolve_aed_active_modules(active_modules)

  if (length(active_modules) > 0) {
    aed_nml <- set_nml(aed_nml, arg_name = "models", arg_val = active_modules)
  }

  # --- Full list of state variables Simstrat will require files for -----
  # Same AED module -> state-variable mapping initialise_aed() (GLM) relies
  # on via its aed.nml template -- no aed_carbon/CAR_* split the way AED2
  # has it (see .aed_module_deps docs).
  base_statevars <- list(
    aed_oxygen         = "OXY_oxy",
    aed_silica         = "SIL_rsi",
    aed_nitrogen       = c("NIT_amm", "NIT_nit"),
    aed_phosphorus     = "PHS_frp",
    aed_organic_matter = c("OGM_doc", "OGM_poc", "OGM_don", "OGM_pon",
                           "OGM_dop", "OGM_pop")
  )
  required_vars <- unlist(base_statevars[intersect(names(base_statevars), active_modules)])

  # aed_nitrogen's N2O/NO2 pools (simn2o) and aed_organic_matter's
  # recalcitrant pools (simrpools) are switch-activated rather than always
  # present, unlike AED2's fixed variable set -- when on, Simstrat still
  # requires an AED_inflow/<var>_inflow.dat for each (missing ini files just
  # fall back to the namelist default and warn, but a missing inflow file is
  # a fatal Fortran runtime error: "Cannot open file ..._inflow.dat").
  if ("aed_nitrogen" %in% active_modules) {
    simn2o <- tryCatch(as.numeric(get_nml_value(aed_nml, "simn2o")),
                       error = function(e) 0)
    if (isTRUE(simn2o > 0)) {
      required_vars <- c(required_vars, "NIT_n2o", "NIT_no2")
    }
  }
  if ("aed_organic_matter" %in% active_modules) {
    simrpools <- tryCatch(isTRUE(get_nml_value(aed_nml, "simrpools")),
                          error = function(e) FALSE)
    if (simrpools) {
      required_vars <- c(required_vars,
                         "OGM_docr", "OGM_donr", "OGM_dopr", "OGM_cpom")
    }
  }

  # Phyto/zoo group names come from the CSV-based par files (the same format
  # and reader approach initialise_aed() uses for GLM-AED, since both link
  # the same AED library) -- unlike AED2's aed2_phyto_pars.nml/
  # aed2_zoop_pars.nml, which use Fortran derived-type `%`-syntax and need
  # the separate read_aed2_group_names() reader (see initialise_aed2()).
  if ("aed_phytoplankton" %in% active_modules) {
    phy_csv_file <- basename(aed_nml[["aed_phytoplankton"]][["dbase"]])
    phy_vals <- readr::read_csv(file.path(path_aed, phy_csv_file),
                                col_types = readr::cols())
    phyto_names <- names(phy_vals)[!grepl("p_name", names(phy_vals))]
    the_phytos <- as.numeric(get_nml_value(aed_nml, "the_phytos"))
    active_phytos <- phyto_names[the_phytos]
    required_vars <- c(required_vars,
                       paste0("PHY_", active_phytos),
                       paste0("PHY_", active_phytos, "_IN"),
                       paste0("PHY_", active_phytos, "_IP"))
  }
  if ("aed_zooplankton" %in% active_modules) {
    zoo_csv_file <- basename(aed_nml[["aed_zooplankton"]][["dbase"]])
    zoo_vals <- readr::read_csv(file.path(path_aed, zoo_csv_file),
                                col_types = readr::cols())
    zoop_names <- names(zoo_vals)[!grepl("zoop_name", names(zoo_vals))]
    the_zoops <- as.numeric(get_nml_value(aed_nml, "the_zoops"))
    active_zoops <- zoop_names[the_zoops]
    required_vars <- c(required_vars, paste0("ZOO_", active_zoops))
  }

  # --- Placeholder inflow files for any required var not already written -
  # Every AED state variable that gets registered (see the module lists
  # above) needs its own AED_inflow/<name>_inflow.dat -- Simstrat's
  # strat_lateral.f90 opens each with status='old' and no iostat=, so a
  # missing file is a hard runtime abort, not a graceful fallback (unlike
  # a missing AED_initcond file, which just falls back to the aed.nml
  # default and warns). Value defaults to model_controls$inf_default for
  # the AED name's var_aeme (converted to AED units the same way
  # initial_wc is below), or 0 if there's no key_naming/model_controls
  # entry for it (e.g. NIT_n2o/NIT_no2/OGM_cpom have none).
  aed_inflow_default <- function(v) {
    kn_idx <- match(v, key_naming$simstrat_aed)
    if (is.na(kn_idx)) return(0)
    mc_idx <- match(key_naming$var_aeme[kn_idx], model_controls$var_aeme)
    if (is.na(mc_idx)) return(0)
    default_val <- model_controls$inf_default[mc_idx]
    conv <- model_controls$conversion_aed[mc_idx]
    if (is.na(default_val) || is.na(conv) || conv == 0) return(0)
    round(default_val / conv, 4)
  }

  inflow_dir <- file.path(path_aed, "AED_inflow")
  for (v in required_vars) {
    inflow_file <- file.path(inflow_dir, paste0(v, "_inflow.dat"))
    if (!file.exists(inflow_file)) {
      val <- aed_inflow_default(v)
      .write_simstrat_grid_file(
        df = data.frame(Date = date_range, value = c(val, val)),
        file = inflow_file,
        comment = "depth [m], conc. [mmol/m3 * m2/s]",
        depth = 0, ref_year = ref_year
      )
    }
  }

  if (sum(is.na(this_ctrls$initial_wc)) > 0) {
    na_vars <- this_ctrls$var_aeme[which(is.na(this_ctrls$initial_wc))]
    cli::cli_abort("Initial concentrations missing for: {paste(na_vars, collapse = ', ')}.
                  Please check your key file")
  }

  for (i in seq_len(nrow(this_ctrls))) {

    var_name <- this_ctrls$var_aeme[i]

    if (grepl("PHY_", var_name) || grepl("ZOO_", var_name)) {
      # Phyto/zoo group parameters live in aed_phyto_pars.csv/
      # aed_zoop_pars.csv, the same CSV-based reader used by GLM-AED
      # (initialise_aed() leaves these untouched too -- see its @details).
      next
    } else if (grepl("CHM_ph", var_name)) {
      next
    }

    nml_param_name <- paste0(gsub("^.*_", "", var_name), "_initial")

    old_val <- tryCatch(
      get_nml_value(glm_nml = aed_nml, arg_name = nml_param_name),
      error = function(e) NA_real_
    )
    if (all(is.na(old_val))) {
      cli_inform_safe(c("!" = paste0(nml_param_name,
                                     " not found in aed.nml, skipping.")))
      next
    }

    new_val <- (this_ctrls$initial_wc[i] / this_ctrls$conversion_aed[i]) |>
      round(4)

    aed_nml <- set_nml(glm_nml = aed_nml, arg_name = nml_param_name,
                       arg_val = new_val)

    msg <- paste0(var_name, ": ", paste0(old_val, " replaced with ", new_val))
    if (round(old_val, 1) != round(new_val, 1)) {
      cli_inform_safe(c("i" = msg))
    }

    ini_lines <- c(
      "depth [m], conc. [mmol/m3]",
      paste(format(0, nsmall = 2), format(new_val, nsmall = 3)),
      paste(format(-max_depth, nsmall = 2), format(new_val, nsmall = 3))
    )
    writeLines(ini_lines, file.path(path_aed, "AED_initcond",
                                    paste0(simstrat_names[i], "_ini.dat")))
  }

  write_nml(aed_nml, aed_nml_file)
  return(invisible())
}
