#' Install DYRESM-CAEDYM executables for AEME
#'
#' Downloads the pre-compiled DYRESM-CAEDYM executable suite (`dycd.exe`
#' plus its three companion tools, `createDYref.exe`, `createDYsim.exe`, and
#' `extractDYinfo.exe`) for the current platform, verifies it against its
#' published SHA256 checksum, and installs it into a persistent user cache
#' directory. Binaries are attached as assets to AEME's GitHub releases,
#' with the version encoded in the asset filename (e.g.
#' `dycd-windows-5.0.0.zip`), mirroring [install_glm_aed()].
#'
#' DYRESM-CAEDYM is closed-source (no public repository to build from), so
#' unlike GLM there is no possibility of compiling it in a CI pipeline --
#' this only ever distributes whatever binary was previously obtained and
#' vetted, exactly as the binaries bundled directly in earlier AEME versions
#' were.
#'
#' @param version Character. The DYRESM-CAEDYM version to install, e.g.
#'   `"5.0.0"`. Use [list_dy_cd_versions()] to see what's available.
#'   Defaults to `"latest"`, which resolves to the highest version number
#'   available for the current platform.
#' @param os Character. One of `"windows"`, `"macos"`, or `"linux"`.
#'   Defaults to the platform R is currently running on; you shouldn't
#'   normally need to set this. Note that only a Windows build exists (no
#'   source is available to build for other platforms), so `macos`/`linux`
#'   will report no versions available.
#' @param repo Character. The `"owner/repo"` GitHub repository that
#'   DYRESM-CAEDYM binaries are attached to. Defaults to
#'   `"limnotrack/AEME"`.
#' @param force Logical. If `FALSE` (the default) and this version is
#'   already installed for this platform, the download/verification steps
#'   are skipped and the existing path is returned. Set to `TRUE` to
#'   re-download and reinstall anyway.
#' @param quiet Logical. If `TRUE`, suppresses progress messages (download
#'   errors and checksum failures still raise, and are never silenced).
#'
#' @return Invisibly, the file path to the installed `dycd.exe` (its three
#'   companion tools sit alongside it in the same directory).
#'
#' @details
#' Binaries are cached under [`tools::R_user_dir("AEME", "data")`][tools::R_user_dir],
#' in an `<os>/<version>/` subdirectory (the same root [install_glm_aed()],
#' [install_gotm_wet()], and [install_simstrat_aed2()] use). Every binary is
#' published alongside a `.sha256` checksum file in the same release. This
#' function downloads both, recomputes the SHA256 of the downloaded zip
#' locally, and compares it to the published value before extracting
#' anything. If the checksums don't match, or if no checksum file is found
#' at all, installation is aborted and nothing is extracted -- this function
#' will not install an unverified binary under any circumstances.
#'
#' @seealso [list_dy_cd_versions()] to discover available versions,
#'   [dy_cd_exe_path()] to locate an already-installed executable,
#'   [install_glm_aed()] for the equivalent GLM-AED installer this mirrors.
#'
#' @examples
#' \dontrun{
#' install_dy_cd(version = "5.0.0")
#'
#' # Force re-download and reinstall
#' install_dy_cd(version = "5.0.0", force = TRUE)
#' }
#'
#' @importFrom digest digest
#' @importFrom gh gh
#' @importFrom cli cli_abort cli_alert_info cli_alert_success cli_alert_warning
#' @importFrom rlang arg_match
#'
#' @export
install_dy_cd <- function(version = "latest",
                          os = NULL,
                          repo = "limnotrack/AEME",
                          force = FALSE,
                          quiet = FALSE) {

  if (!is.character(version) || length(version) != 1L) {
    cli::cli_abort("{.arg version} must be a single character string, e.g. {.val 5.0.0} or {.val latest}.")
  }

  if (is.null(os)) {
    os <- .detect_os()
  } else {
    os <- rlang::arg_match(os, c("windows", "macos", "linux"))
  }

  if (version == "latest") {
    versions <- list_dy_cd_versions(repo = repo, os = os)$dy_cd_version
    if (length(versions) == 0L) {
      cli::cli_abort(c(
        "No DYRESM-CAEDYM binaries found in releases of {.val {repo}} for {.field {os}}.",
        "i" = "Use {.fn list_dy_cd_versions} to see what's available."
      ))
    }
    # Extract latest version
    version <- versions[order(numeric_version(versions), decreasing = TRUE)][1]
    cli::cli_alert_info("Resolved {.val latest} to DYRESM-CAEDYM version {.val {version}}
                        for {.field {os}}.")
  }
  if (missing(version) || !is.character(version) || length(version) != 1L) {
    cli::cli_abort("{.arg version} must be a single character string, e.g.
                   {.val 5.0.0}.")
  }

  exe_name <- if (os == "windows") "dycd.exe" else "dycd"
  install_dir <- file.path(.glm_cache_dir(), os, version)
  exe_path <- file.path(install_dir, exe_name)

  if (file.exists(exe_path) && !force) {
    if (!quiet) {
      cli::cli_alert_info(
        "DYRESM-CAEDYM {.val {version}} ({.field {os}}) is already installed at {.path {install_dir}}.
         Use {.code force = TRUE} to reinstall."
      )
    }
    options(AEME.dyresm_version = version)
    return(invisible(exe_path))
  }

  if (!requireNamespace("gh", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg gh} is required to look up DYRESM-CAEDYM releases.",
      "i" = "Install it with {.run install.packages(\"gh\")}."
    ))
  }
  if (!requireNamespace("digest", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg digest} is required to verify DYRESM-CAEDYM binary checksums.",
      "i" = "Install it with {.run install.packages(\"digest\")}."
    ))
  }

  asset_name <- sprintf("dycd-%s-%s.zip", os, version)
  sha_name <- paste0(asset_name, ".sha256")

  hit <- .glm_find_release_asset(repo, asset_name)
  if (is.null(hit)) {
    cli::cli_abort(c(
      "Could not find a DYRESM-CAEDYM {.val {version}} build for {.field {os}}
       (looked for asset {.file {asset_name}}) in any release of {.val {repo}}.",
      "i" = "Use {.fn list_dy_cd_versions} to see what's available."
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

  tmp_dir <- tempfile("dycd_dl_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  zip_path <- file.path(tmp_dir, asset_name)
  sha_path <- file.path(tmp_dir, sha_name)

  if (!quiet) {
    cli::cli_alert_info(
      "Downloading DYRESM-CAEDYM {.val {version}} for {.field {os}} from release {.val {hit$release_tag}}..."
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
  # The zip contains dycd.exe plus its 3 companion tools (createDYref.exe,
  # createDYsim.exe, extractDYinfo.exe) - all get extracted together.
  utils::unzip(zip_path, exdir = install_dir)

  if (!file.exists(exe_path)) {
    cli::cli_abort(
      "Extraction completed but {.file {exe_name}} was not found in {.path {install_dir}}
       - the release asset may be malformed."
    )
  }

  if (os != "windows") {
    for (f in list.files(install_dir, pattern = "^(dycd|createDYref|createDYsim|extractDYinfo)", full.names = TRUE)) {
      Sys.chmod(f, mode = "0755")
    }
  }
  options(AEME.dyresm_version = version)
  if (!quiet) {
    cli::cli_alert_success("DYRESM-CAEDYM {.val {version}} installed at {.path {install_dir}}")
    # Alert that it is now the default for this R session
    cli::cli_alert_info(
      "DYRESM-CAEDYM {.val {version}} is now the default for this R session.
       Use {.fn dy_cd_exe_path} to locate the executable, or set
       {.code options(AEME.dyresm_exec = <path>)} to override."
    )
  }
  invisible(exe_path)
}

#' List available DYRESM-CAEDYM versions
#'
#' Scans all releases of `repo` and returns every DYRESM-CAEDYM version that
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
#'   `dy_cd_version`. Returns a zero-row data frame (with a message) if
#'   nothing is found.
#'
#' @examples
#' \dontrun{
#' list_dy_cd_versions()
#' list_dy_cd_versions(os = "windows")
#' }
#'
#' @export
list_dy_cd_versions <- function(repo = "limnotrack/AEME", os = NULL) {
  if (!requireNamespace("gh", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg gh} is required.",
      "i" = "Install it with {.run install.packages(\"gh\")}."
    ))
  }
  os <- if (is.null(os)) NULL else match.arg(os, c("windows", "macos", "linux"))

  releases <- .glm_list_releases(repo)
  pattern <- if (is.null(os)) {
    "^dycd-(windows|macos|linux)-(.+)\\.zip$"
  } else {
    sprintf("^dycd-(%s)-(.+)\\.zip$", os)
  }

  rows <- lapply(releases, function(rel) {
    asset_names <- vapply(rel$assets, function(a) a$name, character(1))
    zip_names <- grep(pattern, asset_names, value = TRUE)
    if (length(zip_names) == 0L) return(NULL)
    m <- regmatches(zip_names, regexec(pattern, zip_names))
    data.frame(
      package_release = rel$tag_name,
      os = vapply(m, `[[`, character(1), 2),
      dy_cd_version = vapply(m, `[[`, character(1), 3),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)

  if (is.null(out) || nrow(out) == 0L) {
    cli::cli_alert_warning(
      "No DYRESM-CAEDYM binaries found in releases of {.val {repo}}{if (!is.null(os)) paste0(' for platform ', os) else ''}."
    )
    return(invisible(data.frame(
      package_release = character(), os = character(), dy_cd_version = character()
    )))
  }

  rownames(out) <- NULL
  out[order(out$dy_cd_version, out$os), ]
}

#' Locate an installed DYRESM-CAEDYM executable
#'
#' Returns the path to `dycd.exe` previously installed with
#' [install_dy_cd()] -- its three companion tools (`createDYref.exe`,
#' `createDYsim.exe`, `extractDYinfo.exe`) sit alongside it in the same
#' directory. Errors if that version isn't installed for this platform yet.
#' Mirrors [glm_exe_path()].
#'
#' @param version Character. The DYRESM-CAEDYM version to locate, e.g.
#'   `"5.0.0"`.
#' @param os Character. One of `"windows"`, `"macos"`, or `"linux"`.
#'   Defaults to the current platform.
#'
#' @return Character. The file path to `dycd.exe`.
#'
#' @examples
#' \dontrun{
#' install_dy_cd(version = "5.0.0")
#' dy_cd_exe_path(version = "5.0.0")
#' }
#'
#' @export
dy_cd_exe_path <- function(version = getOption("AEME.dyresm_version", NULL), os = NULL) {
  if (is.null(version)) {
    path <- .resolve_dy_cd_exec()
  } else {
    os <- if (is.null(os)) .detect_os() else match.arg(os, c("windows", "macos", "linux"))
    exe_name <- if (os == "windows") "dycd.exe" else "dycd"
    path <- file.path(.glm_cache_dir(), os, version, exe_name)
  }

  if (!file.exists(path)) {
    cli::cli_abort(c(
      "DYRESM-CAEDYM {.val {version}} is not installed for {.field {os}}.",
      "i" = "Run {.run install_dy_cd(version = \"{version}\")} first."
    ))
  }
  path
}

#' List locally installed DYRESM-CAEDYM versions for a given platform
#' @keywords internal
#' @noRd
.dy_cd_installed_versions <- function(os = .detect_os()) {
  os_dir <- file.path(.glm_cache_dir(), os)
  if (!dir.exists(os_dir)) return(character(0))
  versions <- list.dirs(os_dir, recursive = FALSE, full.names = FALSE)
  exe_name <- if (os == "windows") "dycd.exe" else "dycd"
  versions[file.exists(file.path(os_dir, versions, exe_name))]
}

#' Latest locally installed DYRESM-CAEDYM version for a given platform
#' @keywords internal
#' @noRd
.dy_cd_latest_installed_version <- function(os = .detect_os()) {
  versions <- .dy_cd_installed_versions(os)
  if (length(versions) == 0) return(NULL)
  as.character(max(numeric_version(versions)))
}
