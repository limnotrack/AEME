# Models that only have Windows binaries right now. glm_aed is cross-platform
# (built for windows/linux/macos via install_glm_aed()); dy_cd and gotm_wet
# are not yet. Update this list as they gain cross-platform builds - at that
# point, remove the corresponding entry rather than adding another OS branch
# anywhere that uses it.
.windows_only_models <- c("dy_cd", "gotm_wet")

#' Skip a test entirely if it can't run on this platform
#'
#' Use for tests that only make sense as a whole (e.g. a single-model test,
#' or a "run everything together" test) - if any required model is
#' unavailable, there's no reduced-but-meaningful version of the test to run.
#'
#' Call as early as possible in the test, right after `model` is known,
#' before any expensive setup (file copies, build_aeme(), etc.).
#'
#' @param model Character vector of model names this test requires.
skip_if_models_unavailable <- function(model) {
  needs_windows <- any(model %in% .windows_only_models)
  if (needs_windows && AEME:::.detect_os() != "windows") {
    testthat::skip(paste0(
      "Model(s) ", paste(intersect(model, .windows_only_models), collapse = ", "),
      " only supported on Windows (platform: ", AEME:::.detect_os(), ")"
    ))
  }
  if ("glm_aed" %in% model) {
    skip_if_no_glm()
  }
  invisible(TRUE)
}

#' Reduce a model vector to only the models this platform can actually run
#'
#' Use for tests that are still meaningful with a smaller model set (e.g.
#' "running all models together" tests that would rather run with just the
#' cross-platform-capable subset on non-Windows than skip entirely).
#' Silently drops Windows-only models on non-Windows platforms; returns
#' `model` unchanged on Windows.
#'
#' @param model Character vector of model names.
#' @return The subset of `model` that's runnable on this platform.
filter_platform_models <- function(model) {
  if (AEME:::.detect_os() != "windows") {
    model <- setdiff(model, .windows_only_models)
  }
  model
}

#' Skip a test if a GLM binary can't be obtained for this platform
#'
#' install_glm_aed() is a no-op if this version is already cached, so
#' calling this at the top of every GLM test is cheap after the first call
#' in a session.
#'
#' @param version GLM version to ensure is installed.
skip_if_no_glm <- function(version = getOption("AEME.glm_version", "3.9.108")) {
  exe <- tryCatch(
    suppressMessages(install_glm_aed(version = version, quiet = TRUE)),
    error = function(e) NA_character_
  )
  if (is.na(exe) || !nzchar(exe)) {
    testthat::skip(paste0("GLM ", version, " not available for this platform/environment"))
  }
  invisible(exe)
}

#' Check that every expected model output file exists
#'
#' @param aeme An Aeme object that has already been run.
#' @return Logical; TRUE if all output files exist.
check_all_model_outfiles <- function(aeme) {
  lake_dir <- get_lake_dir(aeme)
  model_outfiles <- get_model_outfile(aeme) |>
    unlist()
  file_chk <- all(file.exists(model_outfiles))
  return(file_chk)
}
