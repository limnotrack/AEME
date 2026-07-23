#' Install a Simstrat-AED2 executable for AEME
#'
#' Downloads a pre-compiled Simstrat (coupled with AED2) binary for the
#' current platform, verifies it against its published SHA256 checksum, and
#' installs it into a persistent user cache directory. Binaries are attached
#' as assets to AEME's GitHub releases, with the Simstrat version encoded in
#' the asset filename (e.g. `simstrat-windows-3.0.4.zip`), mirroring
#' [install_glm_aed()]. Because a given Simstrat version isn't tied to any
#' one AEME release, this function searches across all releases of `repo` to
#' find the release that has the requested version's asset attached.
#'
#' @param version Character. The Simstrat version to install, e.g.
#'   `"3.0.4"`. Use [list_simstrat_aed2_versions()] to see what's available.
#'   Defaults to `"latest"`, which resolves to the highest version number
#'   available for the current platform.
#' @param os Character. One of `"windows"`, `"macos"`, or `"linux"`.
#'   Defaults to the platform R is currently running on; you shouldn't
#'   normally need to set this. Note that only a Windows build is currently
#'   bundled with the package (`inst/extbin/simstrat_aed2/simstrat.exe`) --
#'   `macos`/`linux` release assets don't exist yet, so those platforms will
#'   report no versions available until binaries are published.
#' @param repo Character. The `"owner/repo"` GitHub repository that Simstrat
#'   binaries are attached to. Defaults to `"limnotrack/AEME"`.
#' @param force Logical. If `FALSE` (the default) and this version is
#'   already installed for this platform, the download/verification steps
#'   are skipped and the existing path is returned. Set to `TRUE` to
#'   re-download and reinstall anyway.
#' @param quiet Logical. If `TRUE`, suppresses progress messages (download
#'   errors and checksum failures still raise, and are never silenced).
#'
#' @return Invisibly, the file path to the installed Simstrat-AED2
#'   executable.
#'
#' @details
#' Binaries are cached under [`tools::R_user_dir("AEME", "data")`][tools::R_user_dir],
#' in an `<os>/<version>/` subdirectory (the same root [install_glm_aed()]
#' uses -- the two coexist there because the installed files themselves are
#' named differently, `simstrat`/`simstrat.exe` vs `glm`/`glm.exe`). Multiple
#' Simstrat versions can be installed side by side and switched between via
#' the `version` argument to [simstrat_aed2_exe_path()] without
#' re-downloading.
#'
#' Every binary is published alongside a `.sha256` checksum file in the same
#' release. This function downloads both, recomputes the SHA256 of the
#' downloaded zip locally, and compares it to the published value before
#' extracting anything. If the checksums don't match, or if no checksum file
#' is found at all, installation is aborted and nothing is extracted -- this
#' function will not install an unverified binary under any circumstances.
#'
#' @seealso [list_simstrat_aed2_versions()] to discover available versions,
#'   [simstrat_aed2_exe_path()] to locate an already-installed executable,
#'   [install_glm_aed()] for the equivalent GLM-AED installer this mirrors.
#'
#' @examples
#' \dontrun{
#' install_simstrat_aed2(version = "3.0.4")
#'
#' # Force re-download and reinstall
#' install_simstrat_aed2(version = "3.0.4", force = TRUE)
#' }
#'
#' @importFrom digest digest
#' @importFrom gh gh
#' @importFrom cli cli_abort cli_alert_info cli_alert_success cli_alert_warning
#' @importFrom rlang arg_match
#'
#' @export
install_simstrat_aed2 <- function(version = "latest",
                                  os = NULL,
                                  repo = "limnotrack/AEME",
                                  force = FALSE,
                                  quiet = FALSE) {

  if (!is.character(version) || length(version) != 1L) {
    cli::cli_abort("{.arg version} must be a single character string, e.g. {.val 3.0.4} or {.val latest}.")
  }

  if (is.null(os)) {
    os <- .detect_os()
  } else {
    os <- rlang::arg_match(os, c("windows", "macos", "linux"))
  }

  if (version == "latest") {
    versions <- list_simstrat_aed2_versions(repo = repo, os = os)$simstrat_version
    if (length(versions) == 0L) {
      cli::cli_abort(c(
        "No Simstrat-AED2 binaries found in releases of {.val {repo}} for {.field {os}}.",
        "i" = "Use {.fn list_simstrat_aed2_versions} to see what's available."
      ))
    }
    # Extract latest version
    version <- versions[order(numeric_version(versions), decreasing = TRUE)][1]
    cli::cli_alert_info("Resolved {.val latest} to Simstrat-AED2 version {.val {version}}
                        for {.field {os}}.")
  }
  if (missing(version) || !is.character(version) || length(version) != 1L) {
    cli::cli_abort("{.arg version} must be a single character string, e.g.
                   {.val 3.0.4}.")
  }

  exe_name <- if (os == "windows") "simstrat.exe" else "simstrat"
  install_dir <- file.path(.glm_cache_dir(), os, version)
  exe_path <- file.path(install_dir, exe_name)

  if (file.exists(exe_path) && !force) {
    if (!quiet) {
      cli::cli_alert_info(
        "Simstrat-AED2 {.val {version}} ({.field {os}}) is already installed at {.path {install_dir}}.
         Use {.code force = TRUE} to reinstall."
      )
    }
    options(AEME.simstrat_version = version)
    return(invisible(exe_path))
  }

  if (!requireNamespace("gh", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg gh} is required to look up Simstrat-AED2 releases.",
      "i" = "Install it with {.run install.packages(\"gh\")}."
    ))
  }
  if (!requireNamespace("digest", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg digest} is required to verify Simstrat-AED2 binary checksums.",
      "i" = "Install it with {.run install.packages(\"digest\")}."
    ))
  }

  asset_name <- sprintf("simstrat-%s-%s.zip", os, version)
  sha_name <- paste0(asset_name, ".sha256")

  hit <- .glm_find_release_asset(repo, asset_name)
  if (is.null(hit)) {
    cli::cli_abort(c(
      "Could not find a Simstrat-AED2 {.val {version}} build for {.field {os}}
       (looked for asset {.file {asset_name}}) in any release of {.val {repo}}.",
      "i" = "Use {.fn list_simstrat_aed2_versions} to see what's available."
    ))
  }

  sha_hit <- .glm_find_release_asset(repo, sha_name)
  if (is.null(sha_hit)) {
    cli::cli_abort(c(
      "Found {.file {asset_name}} in release {.val {hit$release_tag}} but no
       matching checksum file ({.file {sha_name}}).",
      "x" = "Refusing to install an unverifiable binary."
    ))
  }

  tmp_dir <- tempfile("simstrat_dl_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  zip_path <- file.path(tmp_dir, asset_name)
  sha_path <- file.path(tmp_dir, sha_name)

  if (!quiet) {
    cli::cli_alert_info(
      "Downloading Simstrat-AED2 {.val {version}} for {.field {os}} from release {.val {hit$release_tag}}..."
    )
  }
  utils::download.file(hit$asset$browser_download_url, zip_path,
                       mode = "wb", quiet = quiet)
  utils::download.file(sha_hit$asset$browser_download_url, sha_path,
                       mode = "wb", quiet = TRUE)

  expected <- .glm_read_checksum(sha_path)
  actual <- tolower(digest::digest(zip_path, algo = "sha256", file = TRUE))

  if (!identical(actual, expected)) {
    cli::cli_abort(c(
      "Checksum verification failed for {.file {asset_name}}.",
      "x" = "expected: {.val {expected}}",
      "x" = "actual:   {.val {actual}}",
      "i" = "The download may be corrupted or tampered with - nothing was installed."
    ))
  }
  if (!quiet) cli::cli_alert_success("Checksum verified.")

  dir.create(install_dir, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(zip_path, exdir = install_dir)

  if (!file.exists(exe_path)) {
    cli::cli_abort(
      "Extraction completed but {.file {exe_name}} was not found in {.path {install_dir}}
       - the release asset may be malformed."
    )
  }

  if (os != "windows") {
    Sys.chmod(exe_path, mode = "0755")
  }
  options(AEME.simstrat_version = version)
  if (!quiet) {
    cli::cli_alert_success("Simstrat-AED2 {.val {version}} installed at {.path {install_dir}}")
    # Alert that it is now the default for this R session
    cli::cli_alert_info(
      "Simstrat-AED2 {.val {version}} is now the default for this R session.
       Use {.fn simstrat_aed2_exe_path} to locate the executable, or set
       {.code options(AEME.simstrat_exec = <path>)} to override."
    )
  }
  invisible(exe_path)
}

#' List available Simstrat-AED2 versions
#'
#' Scans all releases of `repo` and returns every Simstrat-AED2 version that
#' has a binary attached, along with which AEME release (package version)
#' and platform each one belongs to. Mirrors [list_glm_versions()].
#'
#' @param repo Character. The `"owner/repo"` GitHub repository to search.
#'   Defaults to `"limnotrack/AEME"`.
#' @param os Character or `NULL`. If supplied, one of `"windows"`,
#'   `"macos"`, or `"linux"`, restricting results to that platform. If
#'   `NULL` (the default), all platforms are included.
#'
#' @return A data frame with one row per available (release, platform,
#'   version) combination, with columns `package_release`, `os`, and
#'   `simstrat_version`. Returns a zero-row data frame (with a message) if
#'   nothing is found.
#'
#' @examples
#' \dontrun{
#' list_simstrat_aed2_versions()
#' list_simstrat_aed2_versions(os = "windows")
#' }
#'
#' @export
list_simstrat_aed2_versions <- function(repo = "limnotrack/AEME", os = NULL) {
  if (!requireNamespace("gh", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg gh} is required.",
      "i" = "Install it with {.run install.packages(\"gh\")}."
    ))
  }
  os <- if (is.null(os)) NULL else match.arg(os, c("windows", "macos", "linux"))

  releases <- .glm_list_releases(repo)
  pattern <- if (is.null(os)) {
    "^simstrat-(windows|macos|linux)-(.+)\\.zip$"
  } else {
    sprintf("^simstrat-(%s)-(.+)\\.zip$", os)
  }

  rows <- lapply(releases, function(rel) {
    asset_names <- vapply(rel$assets, function(a) a$name, character(1))
    zip_names <- grep(pattern, asset_names, value = TRUE)
    if (length(zip_names) == 0L) return(NULL)
    m <- regmatches(zip_names, regexec(pattern, zip_names))
    data.frame(
      package_release = rel$tag_name,
      os = vapply(m, `[[`, character(1), 2),
      simstrat_version = vapply(m, `[[`, character(1), 3),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)

  if (is.null(out) || nrow(out) == 0L) {
    cli::cli_alert_warning(
      "No Simstrat-AED2 binaries found in releases of {.val {repo}}{if (!is.null(os)) paste0(' for platform ', os) else ''}."
    )
    return(invisible(data.frame(
      package_release = character(), os = character(), simstrat_version = character()
    )))
  }

  rownames(out) <- NULL
  out[order(out$simstrat_version, out$os), ]
}

#' Locate an installed Simstrat-AED2 executable
#'
#' Returns the path to a Simstrat-AED2 executable previously installed with
#' [install_simstrat_aed2()]. Errors if that version isn't installed for
#' this platform yet. Mirrors [glm_exe_path()]; unlike GLM, if no `version`
#' is given this falls back to the binary bundled with the package (see
#' `inst/extbin/simstrat_aed2/`) rather than an installed-version lookup,
#' since that's what [run_aeme()] itself resolves to by default.
#'
#' @param version Character. The Simstrat-AED2 version to locate, e.g.
#'   `"3.0.4"`. If `NULL` (the default), returns the binary bundled with the
#'   package instead of one installed via [install_simstrat_aed2()].
#' @param os Character. One of `"windows"`, `"macos"`, or `"linux"`.
#'   Defaults to the current platform.
#'
#' @return Character. The file path to the `simstrat`/`simstrat.exe`
#'   executable.
#'
#' @examples
#' \dontrun{
#' install_simstrat_aed2(version = "3.0.4")
#' simstrat_aed2_exe_path(version = "3.0.4")
#' }
#'
#' @export
simstrat_aed2_exe_path <- function(version = getOption("AEME.simstrat_version", NULL),
                                   os = NULL) {
  os <- if (is.null(os)) .detect_os() else match.arg(os, c("windows", "macos", "linux"))
  exe_name <- if (os == "windows") "simstrat.exe" else "simstrat"

  if (is.null(version)) {
    path <- file.path(system.file("extbin", "simstrat_aed2", package = "AEME"), exe_name)
  } else {
    path <- file.path(.glm_cache_dir(), os, version, exe_name)
  }

  if (!file.exists(path)) {
    cli::cli_abort(c(
      "Simstrat-AED2 {.val {version %||% 'bundled'}} is not available for {.field {os}} at {.path {path}}.",
      "i" = if (is.null(version)) {
        "The package's bundled binary is missing for this platform."
      } else {
        "Run {.run install_simstrat_aed2(version = \"{version}\")} first."
      }
    ))
  }
  path
}
