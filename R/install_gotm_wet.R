#' Install a GOTM-WET executable for AEME
#'
#' Downloads a pre-compiled GOTM (coupled with the WET biogeochemical
#' library) binary for the current platform, verifies it against its
#' published SHA256 checksum, and installs it into a persistent user cache
#' directory. Binaries are attached as assets to AEME's GitHub releases, with
#' the GOTM-WET version encoded in the asset filename (e.g.
#' `gotm-windows-2023.2.0.zip`), mirroring [install_glm_aed()]. Because a
#' given GOTM-WET version isn't tied to any one AEME release, this function
#' searches across all releases of `repo` to find the release that has the
#' requested version's asset attached.
#'
#' @param version Character. The GOTM-WET version to install, e.g.
#'   `"2023.2.0"`. Use [list_gotm_wet_versions()] to see what's available.
#'   Defaults to `"latest"`, which resolves to the highest version number
#'   available for the current platform.
#' @param os Character. One of `"windows"`, `"macos"`, or `"linux"`.
#'   Defaults to the platform R is currently running on; you shouldn't
#'   normally need to set this. Note that only a Windows build currently
#'   exists (no source is compiled for other platforms), so `macos`/`linux`
#'   will report no versions available.
#' @param repo Character. The `"owner/repo"` GitHub repository that GOTM-WET
#'   binaries are attached to. Defaults to `"limnotrack/AEME"`.
#' @param force Logical. If `FALSE` (the default) and this version is
#'   already installed for this platform, the download/verification steps
#'   are skipped and the existing path is returned. Set to `TRUE` to
#'   re-download and reinstall anyway.
#' @param quiet Logical. If `TRUE`, suppresses progress messages (download
#'   errors and checksum failures still raise, and are never silenced).
#'
#' @return Invisibly, the file path to the installed GOTM-WET executable.
#'
#' @details
#' Binaries are cached under [`tools::R_user_dir("AEME", "data")`][tools::R_user_dir],
#' in an `<os>/<version>/` subdirectory (the same root [install_glm_aed()]
#' and [install_simstrat_aed2()] use -- these coexist there because the
#' installed files themselves are named differently, `gotm`/`gotm.exe` vs
#' `glm`/`glm.exe` vs `simstrat`/`simstrat.exe`).
#'
#' Every binary is published alongside a `.sha256` checksum file in the same
#' release. This function downloads both, recomputes the SHA256 of the
#' downloaded zip locally, and compares it to the published value before
#' extracting anything. If the checksums don't match, or if no checksum file
#' is found at all, installation is aborted and nothing is extracted -- this
#' function will not install an unverified binary under any circumstances.
#'
#' @seealso [list_gotm_wet_versions()] to discover available versions,
#'   [gotm_wet_exe_path()] to locate an already-installed executable,
#'   [install_glm_aed()] for the equivalent GLM-AED installer this mirrors.
#'
#' @examples
#' \dontrun{
#' install_gotm_wet(version = "2023.2.0")
#'
#' # Force re-download and reinstall
#' install_gotm_wet(version = "2023.2.0", force = TRUE)
#' }
#'
#' @importFrom digest digest
#' @importFrom gh gh
#' @importFrom cli cli_abort cli_alert_info cli_alert_success cli_alert_warning
#' @importFrom rlang arg_match
#'
#' @export
install_gotm_wet <- function(version = "latest",
                             os = NULL,
                             repo = "limnotrack/AEME",
                             force = FALSE,
                             quiet = FALSE) {

  if (!is.character(version) || length(version) != 1L) {
    cli::cli_abort("{.arg version} must be a single character string, e.g. {.val 2023.2.0} or {.val latest}.")
  }

  if (is.null(os)) {
    os <- .detect_os()
  } else {
    os <- rlang::arg_match(os, c("windows", "macos", "linux"))
  }

  if (version == "latest") {
    versions <- list_gotm_wet_versions(repo = repo, os = os)$gotm_version
    if (length(versions) == 0L) {
      cli::cli_abort(c(
        "No GOTM-WET binaries found in releases of {.val {repo}} for {.field {os}}.",
        "i" = "Use {.fn list_gotm_wet_versions} to see what's available."
      ))
    }
    # Extract latest version
    version <- versions[order(numeric_version(versions), decreasing = TRUE)][1]
    cli::cli_alert_info("Resolved {.val latest} to GOTM-WET version {.val {version}}
                        for {.field {os}}.")
  }
  if (missing(version) || !is.character(version) || length(version) != 1L) {
    cli::cli_abort("{.arg version} must be a single character string, e.g.
                   {.val 2023.2.0}.")
  }

  exe_name <- if (os == "windows") "gotm.exe" else "gotm"
  install_dir <- file.path(.glm_cache_dir(), os, version)
  exe_path <- file.path(install_dir, exe_name)

  if (file.exists(exe_path) && !force) {
    if (!quiet) {
      cli::cli_alert_info(
        "GOTM-WET {.val {version}} ({.field {os}}) is already installed at {.path {install_dir}}.
         Use {.code force = TRUE} to reinstall."
      )
    }
    options(AEME.gotm_version = version)
    return(invisible(exe_path))
  }

  if (!requireNamespace("gh", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg gh} is required to look up GOTM-WET releases.",
      "i" = "Install it with {.run install.packages(\"gh\")}."
    ))
  }
  if (!requireNamespace("digest", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg digest} is required to verify GOTM-WET binary checksums.",
      "i" = "Install it with {.run install.packages(\"digest\")}."
    ))
  }

  asset_name <- sprintf("gotm-%s-%s.zip", os, version)
  sha_name <- paste0(asset_name, ".sha256")

  hit <- .glm_find_release_asset(repo, asset_name)
  if (is.null(hit)) {
    cli::cli_abort(c(
      "Could not find a GOTM-WET {.val {version}} build for {.field {os}}
       (looked for asset {.file {asset_name}}) in any release of {.val {repo}}.",
      "i" = "Use {.fn list_gotm_wet_versions} to see what's available."
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

  tmp_dir <- tempfile("gotm_dl_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  zip_path <- file.path(tmp_dir, asset_name)
  sha_path <- file.path(tmp_dir, sha_name)

  if (!quiet) {
    cli::cli_alert_info(
      "Downloading GOTM-WET {.val {version}} for {.field {os}} from release {.val {hit$release_tag}}..."
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
  options(AEME.gotm_version = version)
  if (!quiet) {
    cli::cli_alert_success("GOTM-WET {.val {version}} installed at {.path {install_dir}}")
    # Alert that it is now the default for this R session
    cli::cli_alert_info(
      "GOTM-WET {.val {version}} is now the default for this R session.
       Use {.fn gotm_wet_exe_path} to locate the executable, or set
       {.code options(AEME.gotm_exec = <path>)} to override."
    )
  }
  invisible(exe_path)
}

#' List available GOTM-WET versions
#'
#' Scans all releases of `repo` and returns every GOTM-WET version that has
#' a binary attached, along with which AEME release (package version) and
#' platform each one belongs to. Mirrors [list_glm_versions()].
#'
#' @param repo Character. The `"owner/repo"` GitHub repository to search.
#'   Defaults to `"limnotrack/AEME"`.
#' @param os Character or `NULL`. If supplied, one of `"windows"`,
#'   `"macos"`, or `"linux"`, restricting results to that platform. If
#'   `NULL` (the default), all platforms are included.
#'
#' @return A data frame with one row per available (release, platform,
#'   version) combination, with columns `package_release`, `os`, and
#'   `gotm_version`. Returns a zero-row data frame (with a message) if
#'   nothing is found.
#'
#' @examples
#' \dontrun{
#' list_gotm_wet_versions()
#' list_gotm_wet_versions(os = "windows")
#' }
#'
#' @export
list_gotm_wet_versions <- function(repo = "limnotrack/AEME", os = NULL) {
  if (!requireNamespace("gh", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg gh} is required.",
      "i" = "Install it with {.run install.packages(\"gh\")}."
    ))
  }
  os <- if (is.null(os)) NULL else match.arg(os, c("windows", "macos", "linux"))

  releases <- .glm_list_releases(repo)
  pattern <- if (is.null(os)) {
    "^gotm-(windows|macos|linux)-(.+)\\.zip$"
  } else {
    sprintf("^gotm-(%s)-(.+)\\.zip$", os)
  }

  rows <- lapply(releases, function(rel) {
    asset_names <- vapply(rel$assets, function(a) a$name, character(1))
    zip_names <- grep(pattern, asset_names, value = TRUE)
    if (length(zip_names) == 0L) return(NULL)
    m <- regmatches(zip_names, regexec(pattern, zip_names))
    data.frame(
      package_release = rel$tag_name,
      os = vapply(m, `[[`, character(1), 2),
      gotm_version = vapply(m, `[[`, character(1), 3),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)

  if (is.null(out) || nrow(out) == 0L) {
    cli::cli_alert_warning(
      "No GOTM-WET binaries found in releases of {.val {repo}}{if (!is.null(os)) paste0(' for platform ', os) else ''}."
    )
    return(invisible(data.frame(
      package_release = character(), os = character(), gotm_version = character()
    )))
  }

  rownames(out) <- NULL
  out[order(out$gotm_version, out$os), ]
}

#' Locate an installed GOTM-WET executable
#'
#' Returns the path to a GOTM-WET executable previously installed with
#' [install_gotm_wet()]. Errors if that version isn't installed for this
#' platform yet. Mirrors [glm_exe_path()].
#'
#' @param version Character. The GOTM-WET version to locate, e.g.
#'   `"2023.2.0"`.
#' @param os Character. One of `"windows"`, `"macos"`, or `"linux"`.
#'   Defaults to the current platform.
#'
#' @return Character. The file path to the `gotm`/`gotm.exe` executable.
#'
#' @examples
#' \dontrun{
#' install_gotm_wet(version = "2023.2.0")
#' gotm_wet_exe_path(version = "2023.2.0")
#' }
#'
#' @export
gotm_wet_exe_path <- function(version = getOption("AEME.gotm_version", NULL), os = NULL) {
  if (is.null(version)) {
    path <- .resolve_gotm_exec()
  } else {
    os <- if (is.null(os)) .detect_os() else match.arg(os, c("windows", "macos", "linux"))
    exe_name <- if (os == "windows") "gotm.exe" else "gotm"
    path <- file.path(.glm_cache_dir(), os, version, exe_name)
  }

  if (!file.exists(path)) {
    cli::cli_abort(c(
      "GOTM-WET {.val {version}} is not installed for {.field {os}}.",
      "i" = "Run {.run install_gotm_wet(version = \"{version}\")} first."
    ))
  }
  path
}

#' List locally installed GOTM-WET versions for a given platform
#' @keywords internal
#' @noRd
.gotm_installed_versions <- function(os = .detect_os()) {
  os_dir <- file.path(.glm_cache_dir(), os)
  if (!dir.exists(os_dir)) return(character(0))
  versions <- list.dirs(os_dir, recursive = FALSE, full.names = FALSE)
  exe_name <- if (os == "windows") "gotm.exe" else "gotm"
  versions[file.exists(file.path(os_dir, versions, exe_name))]
}

#' Latest locally installed GOTM-WET version for a given platform
#' @keywords internal
#' @noRd
.gotm_latest_installed_version <- function(os = .detect_os()) {
  versions <- .gotm_installed_versions(os)
  if (length(versions) == 0) return(NULL)
  as.character(max(numeric_version(versions)))
}
