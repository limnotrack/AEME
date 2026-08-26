#' Set initial conditions for a GLM-AED simulation
#'
#' Thin wrapper for editing the initial temperature/salinity profile and
#' water-quality initial values of a GLM-AED hydrodynamic nml file in place,
#' without needing an `aeme` object. Intended for a GLM-AED-only workflow
#' where a user just wants to tweak initial conditions, run the model, and
#' load the output.
#'
#' Existing profile depths (`init_profiles$the_depths`) are left unchanged --
#' `temp`/`salt`/`wq_init` values are recycled (via [rep_len()]) across
#' however many depths are already defined.
#'
#' @param path_glm filepath; directory containing the GLM-AED configuration
#' @param temp numeric; new initial water temperature profile. Recycled to
#' the number of depths in `init_profiles`. `NULL` (default) leaves it
#' unchanged.
#' @param salt numeric; new initial salinity profile, same recycling rule as
#' `temp`. `NULL` (default) leaves it unchanged.
#' @param wq_init named list; new initial values for water quality variables,
#' e.g. `list(NIT_amm = 0.5, CHM_oxy = 300)`. Names must match
#' `init_profiles$wq_names` in the nml file. Each value is recycled across
#' depths. `NULL` (default) leaves water quality initial values unchanged.
#' @param glm_file filepath; path to the nml file to edit. Defaults to the
#' GLM hydrodynamic nml (`glm3.nml`/`glm4.nml`) found in `path_glm` via
#' [find_glm_nml()].
#'
#' @return invisibly, the updated nml object
#' @export
#'
#' @examples
#' \dontrun{
#' set_glm_init(path_glm, temp = seq(20, 10, length.out = 10))
#' set_glm_init(path_glm, wq_init = list(NIT_amm = 0.5, CHM_oxy = 300))
#' }

set_glm_init <- function(path_glm, temp = NULL, salt = NULL, wq_init = NULL,
                         glm_file = find_glm_nml(path_glm)) {

  if (is.null(temp) && is.null(salt) && is.null(wq_init)) {
    cli::cli_abort("Provide at least one of 'temp', 'salt' or 'wq_init'.")
  }

  glm_nml <- read_nml(glm_file)
  ip <- glm_nml$init_profiles
  n_depths <- ip$num_depths

  if (!is.null(temp)) {
    glm_nml <- set_nml(glm_nml, arg_name = "the_temps",
                       arg_val = rep_len(temp, n_depths))
  }

  if (!is.null(salt)) {
    glm_nml <- set_nml(glm_nml, arg_name = "the_sals",
                       arg_val = rep_len(salt, n_depths))
  }

  if (!is.null(wq_init) && length(wq_init) > 0) {
    wq_names <- ip$wq_names
    vals <- matrix(ip$wq_init_vals, nrow = n_depths, ncol = length(wq_names))
    for (v in names(wq_init)) {
      idx <- match(v, wq_names)
      if (is.na(idx)) {
        cli::cli_abort(c(
          "'{v}' not found in 'wq_names'.",
          "i" = "Available: {paste(wq_names, collapse = ', ')}"
        ))
      }
      vals[, idx] <- rep_len(wq_init[[v]], n_depths)
    }
    glm_nml <- set_nml(glm_nml, arg_name = "wq_init_vals",
                       arg_val = as.vector(vals))
  }

  write_nml(glm_nml, file = glm_file)

  invisible(glm_nml)
}

#' Set initial conditions for a GOTM-WET simulation
#'
#' Thin wrapper for editing the initial temperature/salinity profile files
#' (`inputs/t_prof_file.dat`/`inputs/s_prof_file.dat`) of a GOTM-WET model
#' directory in place, without needing an `aeme` object. Intended for a
#' GOTM-WET-only workflow where a user just wants to tweak initial
#' conditions, run the model, and load the output.
#'
#' The existing profile depths in each `.dat` file are left unchanged --
#' `temp`/`salt` values are recycled (via [rep_len()]) across however many
#' depths are already defined. The `gotm.yaml` surface SST seed
#' (`surface$sst$constant_value`) is updated to match the surface-most
#' `temp` value when `temp` is provided.
#'
#' @param path_gotm filepath; directory containing the GOTM-WET configuration
#' @param temp numeric; new initial water temperature profile. Recycled to
#' the number of depths in `inputs/t_prof_file.dat`. `NULL` (default) leaves
#' it unchanged.
#' @param salt numeric; new initial salinity profile, same recycling rule as
#' `temp`, written to `inputs/s_prof_file.dat`. `NULL` (default) leaves it
#' unchanged.
#' @param gotm_file filepath; path to the yaml file to edit. Defaults to
#' `gotm.yaml` in `path_gotm`.
#'
#' @return invisibly, the updated gotm yaml object
#' @export
#'
#' @examples
#' \dontrun{
#' set_gotm_init(path_gotm, temp = seq(20, 10, length.out = 10))
#' }

set_gotm_init <- function(path_gotm, temp = NULL, salt = NULL,
                          gotm_file = file.path(path_gotm, "gotm.yaml")) {

  if (is.null(temp) && is.null(salt)) {
    cli::cli_abort("Provide at least one of 'temp' or 'salt'.")
  }

  gotm <- yaml::read_yaml(gotm_file)

  if (!is.null(temp)) {
    surf_temp <- .set_gotm_prof_values(
      file.path(path_gotm, "inputs", "t_prof_file.dat"), temp
    )
    gotm$surface$sst$constant_value <- surf_temp
  }

  if (!is.null(salt)) {
    .set_gotm_prof_values(
      file.path(path_gotm, "inputs", "s_prof_file.dat"), salt
    )
  }

  write_yaml(gotm, gotm_file)

  invisible(gotm)
}

#' Overwrite the value column of a GOTM profile `.dat` file, keeping depths
#'
#' @param file filepath; to a `t_prof_file.dat`/`s_prof_file.dat` profile
#' @param values numeric; new values, recycled to the number of depths
#'   already in `file`
#'
#' @return invisibly, the surface-most (first row, depth 0) new value
#' @noRd

.set_gotm_prof_values <- function(file, values) {

  if (!file.exists(file)) {
    cli::cli_abort("Profile file not found: {file}")
  }

  lines <- readLines(file)
  header <- lines[1]
  depths <- as.numeric(vapply(strsplit(lines[-1], "\\s+"), `[`, character(1), 1))
  n_depths <- length(depths)

  new_vals <- rep_len(values, n_depths)
  out <- c(header, paste(depths, new_vals, sep = "\t"))
  writeLines(out, file)

  invisible(new_vals[1])
}

#' Set initial conditions for a Simstrat simulation
#'
#' Thin wrapper for editing the `InitialConditions.dat` file (temperature/
#' salinity) and, for a Simstrat-AED/AED2 simulation, the per-variable
#' `<var>_ini.dat` override files of a Simstrat model directory in place,
#' without needing an `aeme` object. Intended for a Simstrat-only workflow
#' where a user just wants to tweak initial conditions, run the model, and
#' load the output.
#'
#' The existing profile depths (and `U`, `V`, `k`, `eps` columns) in
#' `InitialConditions.dat` are left unchanged -- `temp`/`salt`/`wq_init`
#' values are recycled (via [rep_len()]) across however many depths are
#' already defined.
#'
#' Water quality initial conditions in Simstrat-AED/AED2 follow a two-layer
#' scheme: the `aed.nml`/`aed2.nml` `<var>_initial` field is a fallback
#' constant used for every grid cell, and an optional per-variable
#' `<path_aed_initial>/<var>_ini.dat` (depth, value) profile -- if present --
#' overrides it, interpolated onto the vertical grid. `wq_init` writes this
#' override file directly (it takes precedence over the nml default
#' regardless of the nml value), rather than editing the nml fallback.
#' Which coupler is in use (Simstrat-AED vs Simstrat-AED2, and therefore
#' the `AED_initcond/`/`AED2_initcond/` directory and variable-naming
#' convention) is detected from whichever of `aed.nml`/`aed2.nml` exists in
#' `path_simstrat`.
#'
#' @param path_simstrat filepath; directory containing the Simstrat
#' configuration
#' @param temp numeric; new initial water temperature profile. Recycled to
#' the number of depths in `InitialConditions.dat`. `NULL` (default) leaves
#' it unchanged.
#' @param salt numeric; new initial salinity profile, same recycling rule as
#' `temp`. `NULL` (default) leaves it unchanged.
#' @param wq_init named list; new initial values for water quality
#' variables, keyed by `var_aeme` name, e.g. `list(NIT_amm = 0.5, CHM_oxy =
#' 300)`. Each value is recycled across the same depths as
#' `InitialConditions.dat` and written to
#' `<AED_initcond|AED2_initcond>/<var>_ini.dat`. `NULL` (default) leaves
#' water quality initial values unchanged. Requires a Simstrat-AED or
#' Simstrat-AED2 configuration (an `aed.nml`/`aed2.nml` in `path_simstrat`).
#'
#' @return invisibly, the updated initial conditions data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' set_simstrat_init(path_simstrat, temp = seq(20, 10, length.out = 10))
#' set_simstrat_init(path_simstrat, wq_init = list(NIT_amm = 0.5, CHM_oxy = 300))
#' }

set_simstrat_init <- function(path_simstrat, temp = NULL, salt = NULL,
                              wq_init = NULL) {

  if (is.null(temp) && is.null(salt) && is.null(wq_init)) {
    cli::cli_abort("Provide at least one of 'temp', 'salt' or 'wq_init'.")
  }

  file <- file.path(path_simstrat, "InitialConditions.dat")
  if (!file.exists(file)) {
    cli::cli_abort("Initial conditions file not found: {file}")
  }

  prof <- read.table(file, skip = 1,
                     col.names = c("depth", "U", "V", "temperature", "salt",
                                   "k", "eps"))
  n_depths <- nrow(prof)

  if (!is.null(temp)) {
    prof$temperature <- rep_len(temp, n_depths)
  }
  if (!is.null(salt)) {
    prof$salt <- rep_len(salt, n_depths)
  }

  lines <- c(
    "Depth [m]    U [m/s]    V [m/s]    T [degC]    S [ppt]    k [J/kg]    eps [W/kg]",
    paste(
      format(prof$depth, nsmall = 2),
      format(prof$U, nsmall = 3),
      format(prof$V, nsmall = 3),
      format(prof$temperature, nsmall = 3),
      format(prof$salt, nsmall = 3),
      format(prof$k, scientific = TRUE),
      format(prof$eps, scientific = TRUE)
    )
  )
  writeLines(lines, file)

  if (!is.null(wq_init) && length(wq_init) > 0) {
    .set_simstrat_wq_init(path_simstrat = path_simstrat, wq_init = wq_init,
                          depths = prof$depth)
  }

  invisible(prof)
}

#' Write Simstrat-AED/AED2 `<var>_ini.dat` initial-condition override files
#'
#' @param path_simstrat filepath; directory containing the Simstrat
#' configuration
#' @param wq_init named list; new initial values for water quality
#'   variables, keyed by `var_aeme` name
#' @param depths numeric; vertical grid depths (negative-down, 0 at
#'   surface) to write each profile against, e.g. from
#'   `InitialConditions.dat`
#'
#' @return invisibly, `NULL`
#' @noRd

.set_simstrat_wq_init <- function(path_simstrat, wq_init, depths) {

  # BGC files live in a subdirectory of path_simstrat (see build_simstrat());
  # fall back to path_simstrat itself for older, unnested layouts.
  if (file.exists(file.path(path_simstrat, "aed2", "aed2.nml"))) {
    bgc_tag <- "AED2"
    type_output <- "simstrat_aed2"
    bgc_dir <- file.path(path_simstrat, "aed2")
  } else if (file.exists(file.path(path_simstrat, "aed", "aed.nml"))) {
    bgc_tag <- "AED"
    type_output <- "simstrat_aed"
    bgc_dir <- file.path(path_simstrat, "aed")
  } else if (file.exists(file.path(path_simstrat, "aed2.nml"))) {
    bgc_tag <- "AED2"
    type_output <- "simstrat_aed2"
    bgc_dir <- path_simstrat
  } else if (file.exists(file.path(path_simstrat, "aed.nml"))) {
    bgc_tag <- "AED"
    type_output <- "simstrat_aed"
    bgc_dir <- path_simstrat
  } else {
    cli::cli_abort(c(
      "No 'aed.nml' or 'aed2.nml' found in {path_simstrat}.",
      "i" = "'wq_init' requires a Simstrat-AED or Simstrat-AED2 configuration."
    ))
  }

  initcond_dir <- file.path(bgc_dir, paste0(bgc_tag, "_initcond"))
  dir.create(initcond_dir, recursive = TRUE, showWarnings = FALSE)

  var_names <- names(wq_init)
  simstrat_names <- rename_modelvars(var_names, type_output = type_output,
                                     warn_unmatched = TRUE)

  n_depths <- length(depths)
  for (i in seq_along(var_names)) {
    if (is.na(simstrat_names[i]) || simstrat_names[i] == "") next

    vals <- rep_len(wq_init[[var_names[i]]], n_depths)
    ini_lines <- c(
      "depth [m], conc. [mmol/m3]",
      paste(format(depths, nsmall = 2), format(vals, nsmall = 3))
    )
    writeLines(ini_lines, file.path(initcond_dir,
                                    paste0(simstrat_names[i], "_ini.dat")))
  }

  invisible(NULL)
}
