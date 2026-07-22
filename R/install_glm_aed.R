#' Install a GLM executable for AEME
#'
#' Downloads a pre-compiled GLM binary for the current platform, verifies it
#' against its published SHA256 checksum, and installs it into a persistent
#' user cache directory. Binaries are attached as assets to AEME's GitHub
#' releases (tagged with the AEME package version, e.g. `"0.4.0"`), with the
#' GLM version encoded in the asset filename (e.g. `glm-windows-3.9.108.zip`).
#' Because a given GLM version isn't tied to any one AEME release, this
#' function searches across all releases of `repo` to find the release that
#' has the requested version's asset attached.
#'
#' @param version Character. The GLM version to install, e.g. `"3.9.108"`.
#'   Use [list_glm_versions()] to see what's available. Defaults to `"latest"`,
#'    which resolves to the highest version number available for the current 
#'    platform.
#' @param os Character. One of `"windows"`, `"macos"`, or `"linux"`.
#'   Defaults to the platform R is currently running on; you shouldn't
#'   normally need to set this.
#' @param repo Character. The `"owner/repo"` GitHub repository that GLM
#'   binaries are attached to. Defaults to `"limnotrack/AEME"`.
#' @param force Logical. If `FALSE` (the default) and this version is
#'   already installed for this platform, the download/verification steps
#'   are skipped and the existing path is returned. Set to `TRUE` to
#'   re-download and reinstall anyway.
#' @param quiet Logical. If `TRUE`, suppresses progress messages (download
#'   errors and checksum failures still raise, and are never silenced).
#'
#' @return Invisibly, the file path to the installed GLM executable.
#'
#' @details
#' Binaries are cached under [`tools::R_user_dir("AEME", "data")`][tools::R_user_dir],
#' in a `<os>/<version>/` subdirectory - by default this resolves to
#' `~/.local/share/R/AEME` on Linux, `~/Library/Application Support/org.R-project.R/R/AEME`
#' on macOS, and `%APPDATA%\R\data\R\AEME` on Windows (overridable via the
#' `R_USER_DATA_DIR`/`XDG_DATA_HOME` environment variables - see
#' [tools::R_user_dir] for details). Multiple GLM versions can be installed
#' side by side and switched between via the `version` argument to
#' [glm_exe_path()] without re-downloading.
#'
#' Every binary is published alongside a `.sha256` checksum file in the same
#' release. This function downloads both, recomputes the SHA256 of the
#' downloaded zip locally, and compares it to the published value before
#' extracting anything. If the checksums don't match, or if no checksum file
#' is found at all, installation is aborted and nothing is extracted - this
#' function will not install an unverified binary under any circumstances.
#'
#' @seealso [list_glm_versions()] to discover available versions,
#'   [glm_exe_path()] to locate an already-installed executable.
#'
#' @examples
#' \dontrun{
#' install_glm_aed(version = "3.9.108")
#'
#' # Force re-download and reinstall
#' install_glm_aed(version = "3.9.108", force = TRUE)
#' }
#' 
#' @importFrom digest digest
#' @importFrom gh gh
#' @importFrom cli cli_abort cli_alert_info cli_alert_success cli_alert_warning
#' @importFrom rlang arg_match
#'
#' @export
install_glm_aed <- function(version = "latest",
                            os = NULL,
                            repo = "limnotrack/AEME",
                            force = FALSE,
                            quiet = FALSE) {
  
  if (!is.character(version) || length(version) != 1L) {
    cli::cli_abort("{.arg version} must be a single character string, e.g. {.val 3.9.108} or {.val latest}.")
  }
  
  if (is.null(os)) {
    os <- .glm_detect_os()
  } else {
    os <- rlang::arg_match(os, c("windows", "macos", "linux"))
  }
  
  if (version == "latest") {
    versions <- list_glm_versions(repo = repo, os = os)$glm_version
    if (length(versions) == 0L) {
      cli::cli_abort(c(
        "No GLM binaries found in releases of {.val {repo}} for {.field {
os}}.",
        "i" = "Use {.fn list_glm_versions} to see what's available."
      ))
    }
    # Extract latest version
    version <- versions[order(numeric_version(versions), decreasing = TRUE)][1]
    cli::cli_alert_info("Resolved {.val latest} to GLM version {.val {version}} 
                        for {.field {os}}.")
  }
  if (missing(version) || !is.character(version) || length(version) != 1L) {
    cli::cli_abort("{.arg version} must be a single character string, e.g. 
                   {.val 3.9.108}.")
  }
  
  exe_name <- if (os == "windows") "glm.exe" else "glm"
  install_dir <- file.path(.glm_cache_dir(), os, version)
  exe_path <- file.path(install_dir, exe_name)
  
  if (file.exists(exe_path) && !force) {
    if (!quiet) {
      cli::cli_alert_info(
        "GLM {.val {version}} ({.field {os}}) is already installed at {.path {install_dir}}.
         Use {.code force = TRUE} to reinstall."
      )
    }
    options(AEME.glm_version = version)
    return(invisible(exe_path))
  }
  
  if (!requireNamespace("gh", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg gh} is required to look up GLM releases.",
      "i" = "Install it with {.run install.packages(\"gh\")}."
    ))
  }
  if (!requireNamespace("digest", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg digest} is required to verify GLM binary checksums.",
      "i" = "Install it with {.run install.packages(\"digest\")}."
    ))
  }
  
  asset_name <- sprintf("glm-%s-%s.zip", os, version)
  sha_name <- paste0(asset_name, ".sha256")
  
  hit <- .glm_find_release_asset(repo, asset_name)
  if (is.null(hit)) {
    cli::cli_abort(c(
      "Could not find a GLM {.val {version}} build for {.field {os}}
       (looked for asset {.file {asset_name}}) in any release of {.val {repo}}.",
      "i" = "Use {.fn list_glm_versions} to see what's available."
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
  
  tmp_dir <- tempfile("glm_dl_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  zip_path <- file.path(tmp_dir, asset_name)
  sha_path <- file.path(tmp_dir, sha_name)
  
  if (!quiet) {
    cli::cli_alert_info(
      "Downloading GLM {.val {version}} for {.field {os}} from release {.val {hit$release_tag}}..."
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
  options(AEME.glm_version = version)
  if (!quiet) {
    cli::cli_alert_success("GLM {.val {version}} installed at {.path {install_dir}}")
    # Alert that it is now the default for this R session
    cli::cli_alert_info(
      "GLM {.val {version}} is now the default for this R session.
       Use {.fn glm_exe_path} to locate the executable, or set {.code options
(AEME.glm_exec = <path>)} to override."
    )
  }
  invisible(exe_path)
}

#' List available GLM versions
#'
#' Scans all releases of `repo` and returns every GLM version that has a
#' binary attached, along with which AEME release (package version) and
#' platform each one belongs to.
#'
#' @param repo Character. The `"owner/repo"` GitHub repository to search.
#'   Defaults to `"limnotrack/AEME"`.
#' @param os Character or `NULL`. If supplied, one of `"windows"`,
#'   `"macos"`, or `"linux"`, restricting results to that platform. If
#'   `NULL` (the default), all platforms are included.
#'
#' @return A data frame with one row per available (release, platform,
#'   version) combination, with columns `package_release`, `os`, and
#'   `glm_version`. Returns a zero-row data frame (with a message) if
#'   nothing is found.
#'
#' @examples
#' \dontrun{
#' list_glm_versions()
#' list_glm_versions(os = "windows")
#' }
#'
#' @export
list_glm_versions <- function(repo = "limnotrack/AEME", os = NULL) {
  if (!requireNamespace("gh", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg gh} is required.",
      "i" = "Install it with {.run install.packages(\"gh\")}."
    ))
  }
  os <- if (is.null(os)) NULL else match.arg(os, c("windows", "macos", "linux"))
  
  releases <- .glm_list_releases(repo)
  pattern <- if (is.null(os)) {
    "^glm-(windows|macos|linux)-(.+)\\.zip$"
  } else {
    sprintf("^glm-(%s)-(.+)\\.zip$", os)
  }
  
  rows <- lapply(releases, function(rel) {
    asset_names <- vapply(rel$assets, function(a) a$name, character(1))
    zip_names <- grep(pattern, asset_names, value = TRUE)
    if (length(zip_names) == 0L) return(NULL)
    m <- regmatches(zip_names, regexec(pattern, zip_names))
    data.frame(
      package_release = rel$tag_name,
      os = vapply(m, `[[`, character(1), 2),
      glm_version = vapply(m, `[[`, character(1), 3),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  
  if (is.null(out) || nrow(out) == 0L) {
    cli::cli_alert_warning(
      "No GLM binaries found in releases of {.val {repo}}{if (!is.null(os)) paste0(' for platform ', os) else ''}."
    )
    return(invisible(data.frame(
      package_release = character(), os = character(), glm_version = character()
    )))
  }
  
  rownames(out) <- NULL
  out[order(out$glm_version, out$os), ]
}

#' Locate an installed GLM executable
#'
#' Returns the path to a GLM executable previously installed with
#' [install_glm_aed()]. Errors if that version isn't installed for this
#' platform yet.
#'
#' @param version Character. The GLM version to locate, e.g. `"3.9.108"`.
#' @param os Character. One of `"windows"`, `"macos"`, or `"linux"`.
#'   Defaults to the current platform.
#'
#' @return Character. The file path to the `glm`/`glm.exe` executable.
#'
#' @examples
#' \dontrun{
#' install_glm_aed(version = "3.9.108")
#' glm_exe_path(version = "3.9.108")
#' }
#'
#' @export
glm_exe_path <- function(version = getOption("AEME.glm_version", NULL), os = NULL) {
  if (is.null(version)) {
    path <- .resolve_glm_exec()
  } else {
    os <- if (is.null(os)) .detect_os() else match.arg(os, c("windows", "macos", "linux"))
    exe_name <- if (os == "windows") "glm.exe" else "glm"
    path <- file.path(.glm_cache_dir(), os, version, exe_name)
  }
  
  if (!file.exists(path)) {
    cli::cli_abort(c(
      "GLM {.val {version}} is not installed for {.field {os}}.",
      "i" = "Run {.run install_glm_aed(version = \"{version}\")} first."
    ))
  }
  path
}

# ---------------------------------------------------------------------------
# Internal helpers (not exported)
# ---------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.glm_cache_dir <- function() {
  tools::R_user_dir("AEME", which = "data")
}

#' @keywords internal
#' @noRd
#' @importFrom gh gh
.glm_list_releases <- function(repo = "limnotrack/AEME") {
  parts <- strsplit(repo, "/", fixed = TRUE)[[1]]
  if (length(parts) != 2L) {
    cli::cli_abort("{.arg repo} must be in the form {.val owner/repo}, got: {.val {repo}}")
  }
  gh::gh("GET /repos/{owner}/{repo}/releases",
         owner = parts[1], repo = parts[2], .limit = Inf)
}

#' Find the release (if any) that has a given asset name attached
#' @keywords internal
#' @noRd
.glm_find_release_asset <- function(repo, asset_name) {
  releases <- .glm_list_releases(repo)
  for (rel in releases) {
    asset_names <- vapply(rel$assets, function(a) a$name, character(1))
    idx <- match(asset_name, asset_names)
    if (!is.na(idx)) {
      return(list(release_tag = rel$tag_name, asset = rel$assets[[idx]]))
    }
  }
  NULL
}

#' Parse a `sha256sum`/`shasum -a 256` formatted checksum file
#' (format: "<hash>  <filename>")
#' @keywords internal
#' @noRd
.glm_read_checksum <- function(sha_path) {
  line <- readLines(sha_path, n = 1L, warn = FALSE)
  hash <- strsplit(trimws(line), "\\s+")[[1]][1]
  if (is.na(hash) || !nzchar(hash)) {
    cli::cli_abort("Could not parse a checksum from {.path {sha_path}}.")
  }
  tolower(hash)
}

#' List locally installed GLM versions for a given platform
#' @keywords internal
#' @noRd
.glm_installed_versions <- function(os = .detect_os()) {
  os_dir <- file.path(.glm_cache_dir(), os)
  if (!dir.exists(os_dir)) return(character(0))
  versions <- list.dirs(os_dir, recursive = FALSE, full.names = FALSE)
  exe_name <- if (os == "windows") "glm.exe" else "glm"
  # Only count a version as "installed" if the executable is actually
  # present - not just a leftover/partial directory from an interrupted
  # or failed install.
  versions[file.exists(file.path(os_dir, versions, exe_name))]
}

#' Latest locally installed GLM version for a given platform
#' @keywords internal
#' @noRd
.glm_latest_installed_version <- function(os = .detect_os()) {
  versions <- .glm_installed_versions(os)
  if (length(versions) == 0) return(NULL)
  as.character(max(numeric_version(versions)))
}
