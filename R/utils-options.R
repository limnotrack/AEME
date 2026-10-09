.onLoad <- function(libname, pkgname) {
  # Set default options when package is loaded
  op <- options()
  op.AEME <- list(
    AEME.inform = TRUE,  # default: messages are shown
    AEME.glm_exec = NULL,
    # Controls whether Simstrat inflow scalar files force the model:
    #   "none" (default) - Tinp/Sinp/AED inflow written but inert
    #   "bgc"            - AED/AED2 inflow concentrations effective, T/S inert
    #   "all"            - T/S effective too (experimental: warm surface bias)
    # FALSE == "none", TRUE == "all". See ?make_inf_simstrat and the
    # "Inflow scalar load" section of .write_simstrat_grid_file().
    AEME.simstrat_inflow_load = "none"
  )
  # Only set options that are not already defined
  toset <- !(names(op.AEME) %in% names(op))
  if (any(toset)) options(op.AEME[toset])

  invisible()
}

.onAttach <- function(libname, pkgname) {
  # No model binaries are bundled with the package any more -- nudge users
  # towards install_models() the first time they load AEME with nothing
  # installed yet. Silent if at least one model is already installed, or
  # if AEME.inform has been turned off.
  if (!isTRUE(getOption("AEME.inform", TRUE))) return(invisible())

  tryCatch({
    os <- .detect_os()

    # GLM-AED ships cross-platform binaries; DYRESM-CAEDYM, GOTM-WET, and
    # Simstrat-AED2 only ever have Windows binaries published as release
    # assets (mirrors check_model(os_valid = TRUE)'s windows_only list) --
    # don't check for, or suggest installing, binaries that can never exist
    # on this platform.
    glm_installed <- length(.glm_installed_versions(os)) > 0
    windows_only_installed <- os == "windows" && (
      length(.gotm_installed_versions(os)) > 0 ||
        length(.dy_cd_installed_versions(os)) > 0 ||
        length(.simstrat_installed_versions(os)) > 0
    )

    if (!glm_installed && !windows_only_installed) {
      if (os == "windows") {
        packageStartupMessage(
          "No AEME model binaries are installed yet. Run install_models() to ",
          "download the ones you need (see ?install_models), or install_glm_aed()/",
          "install_gotm_wet()/install_dy_cd()/install_simstrat_aed2() individually."
        )
      } else {
        packageStartupMessage(
          "No AEME model binaries are installed yet. Run install_glm_aed() to ",
          "install GLM-AED (see ?install_glm_aed) -- the only model with ",
          "binaries published for this platform; DYRESM-CAEDYM, GOTM-WET, ",
          "and Simstrat-AED2 are Windows-only."
        )
      }
    }
  }, error = function(e) invisible())  # never block attach on a check failure

  invisible()
}

#' Resolve the `AEME.simstrat_inflow_load` option to a mode string
#'
#' @return one of `"none"`, `"bgc"`, `"all"`. `FALSE`/`NULL` map to
#'   `"none"`, `TRUE` maps to `"all"`; an unrecognised value warns and
#'   falls back to `"none"`.
#' @noRd
.resolve_simstrat_inflow_load <- function() {
  v <- getOption("AEME.simstrat_inflow_load", "none")
  if (is.null(v) || isFALSE(v)) return("none")
  if (isTRUE(v)) return("all")
  v <- tolower(as.character(v)[1])
  if (!v %in% c("none", "bgc", "all")) {
    cli::cli_warn(c(
      "!" = "Unrecognised {.code AEME.simstrat_inflow_load} value {.val {v}}.",
      "i" = "Using {.val none}. Valid: {.val none}, {.val bgc}, {.val all}."
    ))
    return("none")
  }
  v
}

#' Inform messages respecting the global AEME.inform option
#'
#' @param ... arguments passed to cli_inform_safe()
#' @param .envir environment in which to evaluate `{}` expressions in the
#'   message. Defaults to the calling environment, matching `cli::cli_inform()`.
#'   Forwarded explicitly because otherwise `cli` would interpolate against this
#'   wrapper's frame, where the caller's locals do not exist.
#' @export
cli_inform_safe <- function(..., .envir = parent.frame()) {
  if (isTRUE(getOption("AEME.inform", TRUE))) {
    cli::cli_inform(..., .envir = .envir)
  }
}

#' Inform messages respecting the global AEME.inform option
#' 
#' @description
#' Used primarily as an internal helper to safely suppress messages to console.
#' Messages are printed if the global option is set to TRUE: 
#' `options(AEME.inform = TRUE)`
#' 
#' @param ... arguments passed to cli_inform_safe()
#' @param FUN function to use for messaging, default is cli::cli_inform
#' @param indent logical, whether to indent the message, default is FALSE
#' @param .envir environment in which to evaluate `{}` expressions in the
#'   message. Defaults to the calling environment; forwarded to `FUN` when it
#'   accepts a `.envir` argument so interpolation sees the caller's locals
#'   rather than this wrapper's frame.
#' @export
cli_safe <- function(..., FUN = cli::cli_bullets, indent = TRUE,
                     .envir = parent.frame()) {
  if (isTRUE(getOption("AEME.inform", TRUE))) {
    if (indent) {
      d <- cli::cli_div(theme = list(".bullet" = list("margin-left" = 2)))
      on.exit(cli::cli_end(d))
    }
    if (".envir" %in% names(formals(FUN))) {
      FUN(..., .envir = .envir)
    } else {
      FUN(...)
    }
  }
}

#' Inform messages respecting the global AEME.inform option
#'
#' @param ... arguments passed to cli_inform_safe()
#' @noRd
cli_table_safe <- function(...) {
  if (isTRUE(getOption("AEME.inform", TRUE))) {
    cat(..., sep = "\n")
  }
}
