#' Restrict a model's written output to the variables of interest
#'
#' By default every AEME model writes its full state at every output step.
#' When only a few variables are needed - as in calibration or sensitivity
#' analysis, where the objective is computed from one or two variables - the
#' rest is wasted disk I/O (and, for Simstrat and GOTM, wasted files).
#' `set_output_vars()` rewrites the output section of a model's configuration
#' so that only `vars`, plus the handful of internals AEME always needs to
#' read a result back (water level, the depth grid, temperature), are
#' written.
#'
#' The change is made on the in-memory configuration; call
#' \code{\link{write_configuration}} (or \code{\link{build_aeme}}) to write
#' it to disk. \code{\link{build_aeme}}'s `output_vars` argument applies this
#' automatically at build time.
#'
#' @param aeme An `Aeme` object carrying a configuration for `model`.
#' @param model Character. A single model, one of `"glm_aed"`,
#'   `"gotm_wet"`, `"simstrat_aed"`, `"simstrat_aed2"`, `"dy_cd"`.
#' @param vars Character. AEME variable names to keep, e.g.
#'   `c("HYD_temp", "CHM_oxy")`. Mapped to each model's own output names via
#'   \code{\link{key_naming}}; names with no mapping for `model` are dropped
#'   with a warning.
#' @param mass_balance Logical. For `"glm_aed"` only: keep the GLMv4
#'   `&mass_balance` diagnostic CSV? Default `TRUE`. Ignored for other
#'   models.
#' @param ens_n Integer. Ensemble member whose configuration slot is
#'   updated. Default `1`.
#'
#' @return `aeme`, with its `configuration()` updated.
#'
#' @details
#' * **GLM** always writes the full netCDF - its gridded variables cannot be
#'   sub-selected - so this only drops the `&output` `csv_point_*` keys
#'   (disabling the fixed-depth `WQ_*.csv` files) and, when
#'   `mass_balance = FALSE`, the whole `&mass_balance` block (disabling
#'   `mass_balance.csv`). The whole-lake `lake.csv` (`csv_lake_fname`) is
#'   left on: GLM 4.x only writes the netCDF diagnostic scalars
#'   (`lake_level`, ...) while that CSV is open, and AEME needs `lake_level`
#'   to read a GLM result back.
#' * **Simstrat** writes one `*_out.dat` per variable; this sets
#'   `Output$All = FALSE` and lists only the needed variables.
#' * **GOTM** replaces the `/*` (all-variables) output source with an
#'   explicit list.
#' * **DYRESM** has a fixed output form and is returned unchanged.
#'
#' @seealso \code{\link{set_vars_sim}}, \code{\link{write_configuration}},
#'   \code{\link{get_output_vars}}
#' @export
#'
#' @examples
#' aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
#' path <- tempdir()
#' aeme <- build_aeme(path = path, aeme = aeme, model = "glm_aed",
#'                    model_controls = get_model_controls(), ext_elev = 5)
#' aeme <- set_output_vars(aeme, "glm_aed", "HYD_temp", mass_balance = FALSE)
#' write_configuration(aeme, model = "glm_aed", path = path)
set_output_vars <- function(aeme, model, vars, mass_balance = TRUE,
                            ens_n = 1) {

  aeme <- check_aeme(aeme)
  model <- check_model(model = model)
  if (length(model) != 1L) {
    cli::cli_abort("{.arg model} must be a single model name.")
  }
  if (missing(vars) || !is.character(vars) || length(vars) == 0) {
    cli::cli_abort("{.arg vars} must be a non-empty character vector of AEME
                   variable names.")
  }

  cfg <- configuration(aeme)
  hd <- cfg[[model]][["hydrodynamic"]]
  if (is.null(hd)) {
    cli::cli_abort(c("No hydrodynamic configuration for {.val {model}}.",
                     i = "Run {.fn build_aeme} first."))
  }

  hd <- switch(
    model,
    glm_aed       = .set_glm_output_vars(hd, mass_balance = mass_balance),
    gotm_wet      = .set_gotm_output_vars(hd, keep = .map_output_vars(vars, model)),
    simstrat_aed  = ,
    simstrat_aed2 = .set_simstrat_output_vars(hd,
                                              keep = .map_output_vars(vars, model)),
    dy_cd         = {
      cli::cli_inform(c(i = "{.val dy_cd} has a fixed output form;
                        {.fn set_output_vars} leaves it unchanged."))
      hd
    }
  )

  cfg[[model]][["hydrodynamic"]] <- hd
  configuration(aeme) <- cfg
  aeme
}

#' Map AEME variable names to a model's own output names via key_naming.
#'
#' Always includes the small set of internals each model's output reader
#' needs regardless of the objective variables.
#' @noRd
.map_output_vars <- function(vars, model) {

  kn <- get0("key_naming", envir = asNamespace("AEME"))
  if (is.null(kn)) {
    utils::data("key_naming", package = "AEME", envir = environment())
    kn <- get("key_naming", envir = environment())
  }

  mapped <- kn[[model]][match(vars, kn[["var_aeme"]])]
  missing_vars <- vars[is.na(mapped) | !nzchar(mapped)]
  if (length(missing_vars) > 0) {
    cli::cli_warn(c(
      "!" = "No {.val {model}} output name for: {.val {missing_vars}}.",
      "i" = "These are not restricted."
    ))
  }
  mapped <- unique(mapped[!is.na(mapped) & nzchar(mapped)])

  always <- switch(
    model,
    simstrat_aed = ,
    simstrat_aed2 = c("T", "S", "WaterH", "HA", "HW", "HK", "HV", "Rad0"),
    # Everything AEME's GOTM reader touches unconditionally:
    #  - grid / level:      h, z, zi, zeta, sst   (read_gotm_output)
    #  - flux block:         Af, qe, qh, ql, I_0, evap, precip, temp, airt
    #                        (read_gotm_flux_output, incl_fluxes = TRUE)
    #  - salt is kept for the CHM_salt / density path.
    # `time` is a dimension and is always written, so it is not listed.
    # The per-inflow/outflow `Q_*` series are handled gracefully when
    # absent, so they are intentionally not kept.
    gotm_wet = c("temp", "salt", "h", "z", "zi", "zeta", "sst",
                 "Af", "qe", "qh", "ql", "I_0", "evap", "precip", "airt"),
    character(0)
  )
  unique(c(always, mapped))
}

#' GLM: disable the fixed-depth point CSVs, and optionally the mass-balance
#' CSV. The netCDF is always full, and the whole-lake summary CSV
#' (`csv_lake_fname`) is deliberately left on - see below.
#' @noRd
.set_glm_output_vars <- function(hd, mass_balance = TRUE) {

  if (!is.null(hd[["output"]])) {
    # Only the fixed-depth point CSVs (`WQ_*.csv`) are dropped: they are the
    # ones that multiply with variables x depths. GLM disables an output
    # stream when its keys are absent, not when they are blank/zero.
    #
    # `csv_lake_fname` (the one-row-per-step `lake.csv`) is intentionally
    # NOT removed. In GLM 4.x `write_diags()` early-returns when the lake CSV
    # is closed, and `write_glm_diag_ncdf()` - which writes the netCDF
    # diagnostic scalars `lake_level`, `lake_volume`, `surface_area`,
    # `evaporation`, `surface_temp`, the daily heat fluxes, ... - is the last
    # call in `write_diags()`. Dropping `csv_lake_fname` therefore silently
    # blanks `lake_level` in `output.nc`, which AEME's GLM reader needs to
    # build the depth grid (`read_glm_output()` -> `interp_static_grid()`).
    # `lake.csv` is a single small file, so keeping it costs almost nothing.
    for (k in c("csv_point_nlevs", "csv_point_fname", "csv_point_at",
                "csv_point_nvars", "csv_point_vars", "csv_point_frombot",
                "csv_point_depth_avg", "csv_point_zone_upper",
                "csv_point_zone_lower")) {
      hd[["output"]][[k]] <- NULL
    }
  }

  # GLM treats an empty `balance_file` as a filename and fails to create it,
  # so drop the whole block rather than blanking it.
  if (!isTRUE(mass_balance)) hd[["mass_balance"]] <- NULL
  hd
}

#' Simstrat: switch off "write everything" and pin the variable list.
#' @noRd
.set_simstrat_output_vars <- function(hd, keep) {
  if (is.null(hd[["Output"]])) return(hd)
  hd[["Output"]][["All"]] <- FALSE
  hd[["Output"]][["Variables"]] <- as.list(keep)
  hd
}

#' GOTM: replace the `/*` all-variables source with an explicit list, for
#' every output file defined in the yaml.
#' @noRd
.set_gotm_output_vars <- function(hd, keep) {
  if (is.null(hd[["output"]])) return(hd)
  sources <- lapply(keep, function(v) list(source = v))
  for (f in names(hd[["output"]])) {
    if (!is.null(hd[["output"]][[f]][["variables"]])) {
      hd[["output"]][[f]][["variables"]] <- sources
    }
  }
  hd
}
