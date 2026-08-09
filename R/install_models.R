#' Install the latest available binary for every AEME model
#'
#' Convenience wrapper around [install_glm_aed()], [install_gotm_wet()],
#' [install_dy_cd()], and [install_simstrat_aed2()] that installs the latest
#' version of each, for whichever models actually have a binary published
#' for the current platform. A model with no release asset for this OS (or
#' no release assets published at all yet, e.g. GOTM-WET/DYRESM-CAEDYM
#' before their first upload) is reported and skipped rather than aborting
#' the whole call - one missing model shouldn't block installing the
#' others.
#'
#' @param model Character vector of model names to install, in either
#'   display (`"GLM-AED"`) or code (`"glm_aed"`) form. Defaults to every
#'   model AEME knows about (see [list_models()]).
#' @param os Character. One of `"windows"`, `"macos"`, or `"linux"`.
#'   Defaults to the platform R is currently running on.
#' @param repo Character. The `"owner/repo"` GitHub repository release
#'   assets are attached to. Defaults to `"limnotrack/AEME"`.
#' @param force Logical. If `FALSE` (the default), a model already
#'   installed at the resolved "latest" version is left alone. Set to
#'   `TRUE` to re-download and reinstall every model regardless.
#' @param quiet Logical. If `TRUE`, suppresses the per-model progress
#'   messages from each installer (the final summary is still printed
#'   unless silenced separately - see Value).
#'
#' @return Invisibly, a named character vector with one entry per requested
#'   model: the installed executable path on success, or `NA` for any model
#'   that was skipped or failed.
#'
#' @seealso [install_glm_aed()], [install_gotm_wet()], [install_dy_cd()],
#'   [install_simstrat_aed2()] to install a single model with more control
#'   (specific version, etc.).
#'
#' @examples
#' \dontrun{
#' install_models()
#' install_models(model = c("glm_aed", "simstrat_aed2"))
#' install_models(force = TRUE)
#' }
#'
#' @importFrom cli cli_alert_success cli_alert_warning cli_alert_danger
#'
#' @export
install_models <- function(model = NULL,
                           os = NULL,
                           repo = "limnotrack/AEME",
                           force = FALSE,
                           quiet = FALSE) {

  model <- if (is.null(model)) unname(list_models()) else unname(check_model(model))

  installers <- list(
    glm_aed       = install_glm_aed,
    gotm_wet      = install_gotm_wet,
    dy_cd         = install_dy_cd,
    simstrat_aed2 = install_simstrat_aed2
  )

  unknown <- setdiff(model, names(installers))
  if (length(unknown) > 0) {
    cli::cli_alert_warning("No installer available for {.val {unknown}} - skipping.")
    model <- setdiff(model, unknown)
  }

  results <- stats::setNames(rep(NA_character_, length(model)), model)

  for (m in model) {
    display_name <- toggle_models(m, to = "display")
    exe <- tryCatch(
      installers[[m]](version = "latest", os = os, repo = repo,
                      force = force, quiet = quiet),
      error = function(e) {
        cli::cli_alert_warning(
          "Could not install {.field {display_name}}: {conditionMessage(e)}"
        )
        NA_character_
      }
    )
    results[[m]] <- if (is.null(exe)) NA_character_ else exe
  }

  ok <- names(results)[!is.na(results)]
  failed <- setdiff(model, ok)
  cli::cli_rule("Install summary")
  if (length(ok) > 0) {
    cli::cli_alert_success("Installed: {.field {toggle_models(ok, to = 'display')}}")
  }
  if (length(failed) > 0) {
    cli::cli_alert_danger("Not installed: {.field {toggle_models(failed, to = 'display')}}")
  }

  invisible(results)
}
