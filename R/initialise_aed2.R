#' Write initial concentrations and active modules for a Simstrat-AED2 simulation
#'
#' @param model_controls dataframe of loaded model controls.
#' @param path_aed2 filepath; to the directory containing `aed2.nml` (the
#' Simstrat-AED2 directory).
#' @param max_depth numeric; lake depth (m, positive), used to write a
#' top/bottom pair of rows to each `AED2_initcond/<var>_ini.dat` file.
#' @param date_range length-2 Date/POSIXct vector; simulation start/stop,
#' used to write placeholder `AED2_inflow/<var>_inflow.dat` files spanning
#' the whole simulation for any active state variable AEME does not
#' otherwise provide inflow data for.
#' @param ref_year integer; Simstrat `Simulation.Reference year`.
#'
#' @details Simstrat's AED2 coupling requires an `AED2_inflow/<var>_inflow.dat`
#' file to exist for **every** state variable belonging to an active AED2
#' module -- not just the ones AEME explicitly tracks via `model_controls`
#' (verified: Simstrat aborts with a Fortran runtime error, "Cannot open
#' file ... : No such file or directory", if any are missing). This function
#' writes a zero-value placeholder inflow file for every required state
#' variable that \code{\link{make_inf_simstrat}} did not already write from
#' real tributary data, so the run never crashes for this reason.
#' `AED2_initcond/<var>_ini.dat` files are optional (Simstrat falls back to
#' the `aed2.nml` `<var>_initial` default when absent), so those are only
#' written for variables AEME actually configures.
#'
#' Phytoplankton (`PHY_*`) and zooplankton (`ZOO_*`) group parameters live in
#' `aed2_phyto_pars.nml`/`aed2_zoop_pars.nml`, using Fortran derived-type
#' member syntax (e.g. `pd\%p_name`). AEME's generic nml reader/writer
#' (\code{\link{read_nml}}/\code{\link{set_nml}}, see `R/nml_helpers.R`)
#' cannot parse this syntax (verified: reading `aed2_phyto_pars.nml` via
#' `read_nml()` returns `NULL` values), so those two files are left as
#' static copied templates and their group parameters/initial
#' concentrations are not rewritten here -- matching how
#' \code{\link{initialise_aed}} also leaves zooplankton initial
#' concentrations untouched for GLM-AED. Group *names* (needed to know which
#' inflow/initcond files Simstrat requires) are instead extracted with a
#' small regex-based reader (\code{read_aed2_group_names()}) that does not
#' go through the generic nml engine.
#'
#' @return Written `aed2.nml`, `AED2_initcond/<var>_ini.dat`, and
#' `AED2_inflow/<var>_inflow.dat` files.
#' @noRd
#'
#' @importFrom dplyr filter pull
initialise_aed2 <- function(model_controls, path_aed2, max_depth = 10,
                            date_range, ref_year) {

  aed2_nml_file <- file.path(path_aed2, "aed2.nml")
  aed2_nml <- read_nml(aed2_nml_file)

  # Simstrat resolves nml-referenced file paths (dbase, ConfigFile, Path...)
  # relative to simstrat.par's own directory (path_simstrat), not relative to
  # aed2.nml's directory -- confirmed empirically: 'aed2/aed2_phyto_pars.nml'
  # works, a bare 'aed2_phyto_pars.nml' does not, when aed2.nml itself lives
  # in the "aed2" subdirectory (see build_simstrat()). The shipped
  # aed2.nml template hardcodes these as bare filenames, so they must be
  # rewritten with the BGC subdirectory prefix here.
  bgc_subdir <- basename(path_aed2)
  for (grp in c("aed2_phytoplankton", "aed2_zooplankton")) {
    dbase <- aed2_nml[[grp]][["dbase"]]
    if (!is.null(dbase)) {
      aed2_nml[[grp]][["dbase"]] <- file.path(bgc_subdir, basename(dbase))
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
                                   "PHS_tp", "NIT_tn", "PHY_tchla")
    )

  if (nrow(this_ctrls) == 0) {
    cli_inform_safe(c("i" = "No variables to initialise in AED2"))
    write_nml(aed2_nml, aed2_nml_file)
    return(invisible())
  }
  simstrat_names <- rename_modelvars(input = this_ctrls$var_aeme,
                                     type_output = "simstrat_aed2",
                                     warn_unmatched = TRUE)
  keep <- !is.na(simstrat_names) & simstrat_names != ""
  this_ctrls <- this_ctrls[keep, ]
  simstrat_names <- simstrat_names[keep]

  # --- Determine active AED2 modules -----------------------------------
  module_map <- c(OXY = "aed2_oxygen", CAR = "aed2_carbon",
                  SIL = "aed2_silica", NIT = "aed2_nitrogen",
                  PHS = "aed2_phosphorus", OGM = "aed2_organic_matter",
                  PHY = "aed2_phytoplankton", ZOO = "aed2_zooplankton")
  module_order <- c("aed2_oxygen", "aed2_carbon", "aed2_silica",
                    "aed2_nitrogen", "aed2_phosphorus",
                    "aed2_organic_matter", "aed2_phytoplankton",
                    "aed2_zooplankton")

  prefixes <- sub("_.*$", "", simstrat_names)
  active_modules <- module_order[module_order %in% unname(module_map[prefixes])]

  # The shipped aed2.nml template hardcodes cross-module dependencies as
  # "target variable" links (e.g. aed2_phytoplankton's
  # `c_uptake_target_variable = 'CAR_dic'`, `n1_uptake_target_variable =
  # 'NIT_nit'`, `p1_uptake_target_variable = 'PHS_frp'`,
  # `si_uptake_target_variable = 'SIL_rsi'`, `do_uptake_target_variable =
  # 'OXY_oxy'`; aed2_organic_matter similarly references OXY_oxy/NIT_*/
  # PHS_frp). If the referenced module isn't active, Simstrat aborts with
  # "[ERROR] Undefined variable <name>" (verified: reproducibly, not
  # intermittently, whenever aed2_phytoplankton was active without
  # aed2_carbon). model_controls-driven detection alone can't know this, so
  # force-include every module phytoplankton/zooplankton depend on whenever
  # either is active.
  if (any(c("aed2_phytoplankton", "aed2_zooplankton") %in% active_modules)) {
    active_modules <- union(active_modules,
                            c("aed2_oxygen", "aed2_carbon", "aed2_silica",
                              "aed2_nitrogen", "aed2_phosphorus",
                              "aed2_organic_matter"))
    active_modules <- module_order[module_order %in% active_modules]
  }

  if (length(active_modules) > 0) {
    aed2_nml <- set_nml(aed2_nml, arg_name = "models",
                        arg_val = active_modules)
  }

  # --- Full list of state variables Simstrat will require files for -----
  # Fixed per the AED2 library spec (confirmed against a real Simstrat run's
  # "Configured variables to simulate" log output)
  base_statevars <- list(
    aed2_oxygen         = "OXY_oxy",
    aed2_carbon         = c("CAR_dic", "CAR_pH", "CAR_ch4"),
    aed2_silica         = "SIL_rsi",
    aed2_nitrogen       = c("NIT_amm", "NIT_nit"),
    aed2_phosphorus     = "PHS_frp",
    aed2_organic_matter = c("OGM_doc", "OGM_poc", "OGM_don", "OGM_pon",
                            "OGM_dop", "OGM_pop")
  )
  required_vars <- unlist(base_statevars[intersect(names(base_statevars), active_modules)])

  if ("aed2_phytoplankton" %in% active_modules) {
    phyto_names <- read_aed2_group_names(file.path(path_aed2, "aed2_phyto_pars.nml"),
                                         "p_name")
    the_phytos <- as.numeric(get_nml_value(aed2_nml, "the_phytos"))
    active_phytos <- phyto_names[the_phytos]
    required_vars <- c(required_vars,
                       paste0("PHY_", active_phytos),
                       paste0("PHY_", active_phytos, "_IN"),
                       paste0("PHY_", active_phytos, "_IP"))
  }
  if ("aed2_zooplankton" %in% active_modules) {
    zoop_names <- read_aed2_group_names(file.path(path_aed2, "aed2_zoop_pars.nml"),
                                        "zoop_name")
    the_zoops <- as.numeric(get_nml_value(aed2_nml, "the_zoops"))
    active_zoops <- zoop_names[the_zoops]
    required_vars <- c(required_vars, paste0("ZOO_", active_zoops))
  }

  # --- Placeholder inflow files for any required var not already written -
  inflow_dir <- file.path(path_aed2, "AED2_inflow")
  for (v in required_vars) {
    inflow_file <- file.path(inflow_dir, paste0(v, "_inflow.dat"))
    if (!file.exists(inflow_file)) {
      .write_simstrat_grid_file(
        df = data.frame(Date = date_range, value = c(0, 0)),
        file = inflow_file,
        comment = "depth [m], conc. [mmol/m3 * m2/s]",
        depth = 0, ref_year = ref_year,
        integrate = .resolve_simstrat_inflow_load() %in% c("bgc", "all")
      )
    }
  }

  if (nrow(this_ctrls) == 0) {
    cli_inform_safe(c("i" = "No variables to initialise in AED2"))
    write_nml(aed2_nml, aed2_nml_file)
    return(invisible())
  }

  if (sum(is.na(this_ctrls$initial_wc)) > 0) {
    na_vars <- this_ctrls$var_aeme[which(is.na(this_ctrls$initial_wc))]
    cli::cli_abort("Initial concentrations missing for: {paste(na_vars, collapse = ', ')}.
                  Please check your key file")
  }

  for (i in seq_len(nrow(this_ctrls))) {

    var_name <- this_ctrls$var_aeme[i]

    if (grepl("PHY_", var_name) || grepl("ZOO_", var_name)) {
      # Phyto/zoo group parameters use %-syntax nml files that AEME's
      # generic nml reader cannot parse -- see function @details.
      next
    } else if (grepl("CHM_ph", var_name)) {
      next
    }

    nml_param_name <- paste0(gsub("^.*_", "", var_name), "_initial")

    old_val <- tryCatch(
      get_nml_value(glm_nml = aed2_nml, arg_name = nml_param_name),
      error = function(e) NA_real_
    )
    if (all(is.na(old_val))) {
      cli_inform_safe(c("!" = paste0(nml_param_name,
                                     " not found in aed2.nml, skipping.")))
      next
    }

    new_val <- (this_ctrls$initial_wc[i] / this_ctrls$conversion_aed[i]) |>
      round(4)

    aed2_nml <- set_nml(glm_nml = aed2_nml, arg_name = nml_param_name,
                        arg_val = new_val)

    msg <- paste0(var_name, ": ", paste0(old_val, " replaced with ", new_val))
    if (round(old_val, 1) != round(new_val, 1)) {
      cli_inform_safe(c("i" = msg))
    }

    # Explicit top/bottom initial-condition profile file (AED2Config's
    # PathAED2initial), redundant with but taking priority over the nml
    # `<var>_initial` default for state variables Simstrat looks up there.
    ini_lines <- c(
      "depth [m], conc. [mmol/m3]",
      paste(format(0, nsmall = 2), format(new_val, nsmall = 3)),
      paste(format(-max_depth, nsmall = 2), format(new_val, nsmall = 3))
    )
    writeLines(ini_lines, file.path(path_aed2, "AED2_initcond",
                                    paste0(simstrat_names[i], "_ini.dat")))
  }

  write_nml(aed2_nml, aed2_nml_file)
  return(invisible())
}

#' Extract AED2 parameter-group names from a `%`-syntax nml file
#'
#' Small standalone reader for `aed2_phyto_pars.nml`/`aed2_zoop_pars.nml`,
#' which AEME's generic nml engine (\code{\link{read_nml}}) cannot parse
#' because they use Fortran derived-type member syntax (e.g. `pd\%p_name`).
#' This reads the raw text directly rather than going through that engine.
#'
#' @param file path to the nml file.
#' @param name_key character; the field name to extract (e.g. `"p_name"` or
#' `"zoop_name"`).
#'
#' @return character vector of (unquoted) group names, in file order.
#' @noRd
read_aed2_group_names <- function(file, name_key) {
  lines <- readLines(file)
  line <- lines[grepl(paste0("%", name_key, "\\s*="), lines)]
  if (length(line) == 0) {
    cli::cli_abort("Could not find {.val {name_key}} in {.file {file}}.")
  }
  matches <- regmatches(line[1], gregexpr("'([^']*)'", line[1]))[[1]]
  gsub("'", "", matches)
}
