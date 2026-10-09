#' Options for the AED biogeochemistry setup in `build_aeme()`
#'
#' With `use_bgc = TRUE`, [build_aeme()] configures AED for GLM-AED in several
#' steps. `aed_options()` lets you choose which of these run. The defaults
#' reproduce the usual build, so only set what you want to change. Every step
#' is also available on its own once the model is built:
#' [set_glm_aed_models()], [set_aed_sed_const2d()] and [set_aed_totals()].
#'
#' @param modules character or `NULL`; the AED modules to activate, from
#'   `"aed_sedflux"`, `"aed_noncohesive"`, `"aed_oxygen"`, `"aed_silica"`,
#'   `"aed_nitrogen"`, `"aed_phosphorus"`, `"aed_alum"`, `"aed_organic_matter"`,
#'   `"aed_phytoplankton"`, `"aed_zooplankton"`, `"aed_macrophyte"` and
#'   `"aed_totals"`. `NULL` (default) activates the modules needed for the
#'   variables set to simulate in `model_controls`.
#' @param resolve_deps logical; when `modules` is supplied, also activate the
#'   modules they depend on (e.g. `aed_nitrogen` needs `aed_oxygen` and
#'   `aed_sedflux`), as the `model_controls`-driven default does. If `FALSE`,
#'   `modules` is used exactly as given, and GLM aborts at runtime if a
#'   module's prerequisites are missing. Default `TRUE`.
#' @param initialise logical; write the initial concentrations and active
#'   modules to `aed.nml` from `model_controls`. If `FALSE`, `aed.nml` is left
#'   as it is (the shipped template for a new lake, or your own file for an
#'   existing one). Default `TRUE`.
#' @param sed_zones logical; estimate and set the per-zone sediment fluxes
#'   (`aed_sed_const2d`) with [set_aed_sed_const2d()]. Default `TRUE`.
#' @param totals logical; derive the `aed_totals` (TN, TP, TOC and TSS)
#'   variable lists with [set_aed_totals()]. Default `TRUE`.
#'
#' @return An `aed_options` object, to pass as the `aed` argument of
#'   [build_aeme()].
#' @export
#'
#' @seealso [build_aeme()], [set_glm_aed_models()], [set_aed_sed_const2d()],
#'   [set_aed_totals()]
#'
#' @examples
#' # Default: everything as usual
#' aed_options()
#'
#' # Only oxygen and nutrient cycling, and leave the sediment fluxes alone
#' aed_options(
#'   modules = c("aed_sedflux", "aed_oxygen", "aed_nitrogen", "aed_phosphorus"),
#'   sed_zones = FALSE
#' )
aed_options <- function(modules = NULL,
                        resolve_deps = TRUE,
                        initialise = TRUE,
                        sed_zones = TRUE,
                        totals = TRUE) {
  if (!is.null(modules)) {
    if (!is.character(modules) || anyNA(modules)) {
      cli::cli_abort("{.arg modules} must be {.code NULL} or a character vector.")
    }
    unknown <- setdiff(modules, .aed_module_order)
    if (length(unknown) > 0) {
      valid <- .aed_module_order
      cli::cli_abort(c(
        "Unknown AED module{?s} in {.arg modules}: {.val {unknown}}.",
        "i" = "Choose from {.val {valid}}."
      ))
    }
    modules <- unique(modules)
  }
  flags <- list(resolve_deps = resolve_deps, initialise = initialise,
                sed_zones = sed_zones, totals = totals)
  for (nm in names(flags)) {
    if (!is.logical(flags[[nm]]) || length(flags[[nm]]) != 1 ||
        is.na(flags[[nm]])) {
      cli::cli_abort("{.arg {nm}} must be {.code TRUE} or {.code FALSE}.")
    }
  }
  structure(
    list(modules = modules, resolve_deps = resolve_deps,
         initialise = initialise, sed_zones = sed_zones, totals = totals),
    class = "aed_options"
  )
}

#' @export
print.aed_options <- function(x, ...) {
  cli::cli_text("{.cls aed_options}")
  mods <- if (is.null(x$modules)) {
    "from {.arg model_controls}"
  } else if (x$resolve_deps) {
    "{.val {x$modules}} plus their dependencies"
  } else {
    "{.val {x$modules}} exactly as given"
  }
  cli::cli_bullets(c(
    "*" = paste0("Modules: ", mods),
    "*" = "Write initial concentrations to {.file aed.nml}: {.val {x$initialise}}",
    "*" = "Estimate sediment zone fluxes: {.val {x$sed_zones}}",
    "*" = "Derive {.field aed_totals}: {.val {x$totals}}"
  ))
  invisible(x)
}

#' Check and default the `aed` argument of `build_aeme()`
#'
#' @param aed `NULL` (use the defaults), a list of [aed_options()] arguments,
#'   or an `aed_options` object.
#' @return An `aed_options` object.
#' @noRd
check_aed_options <- function(aed) {
  if (is.null(aed)) return(aed_options())
  if (inherits(aed, "aed_options")) return(aed)
  if (is.list(aed)) return(do.call(aed_options, aed))
  cli::cli_abort(c(
    "{.arg aed} must be created with {.fn aed_options}.",
    "x" = "Got {.cls {class(aed)}}."
  ))
}

#' Resolve the AED module list requested by an `aed_options` object
#'
#' @param aed `aed_options` object.
#' @return `NULL` when the modules are left to `model_controls`, otherwise the
#'   character vector of modules to activate.
#' @noRd
aed_options_modules <- function(aed) {
  if (is.null(aed$modules)) return(NULL)
  if (aed$resolve_deps) resolve_aed_active_modules(aed$modules) else aed$modules
}
