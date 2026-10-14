#' Detect the GLM hydrodynamic nml file within a directory
#'
#' GLM's hydrodynamic namelist file has historically been named `glm3.nml`,
#' but newer GLM releases may write e.g. `glm4.nml` instead. AEME treats any
#' file matching `glm<version>.nml` in the top level of a `glm_aed` model
#' directory as *the* hydrodynamic nml, so the rest of the package works the
#' same regardless of which GLM version produced it. If more than one such
#' file is present, the choice between them is resolved by
#' .preferred_glm_major_version() (see Details).
#'
#' @param dir character; the `glm_aed` model directory (not searched
#'   recursively -- AED's own nml files live one level down in `aed/`, so
#'   they never collide with this pattern).
#' @param must_exist logical; abort if none is found. Default `TRUE`.
#'
#' @return character; full path to the matching file, or `NA_character_` if
#'   `must_exist = FALSE` and none was found.
#'
#' @details
#' When multiple `glm<version>.nml` files are found in the same directory
#' (e.g. a leftover `glm3.nml` alongside a newer `glm4.nml`), the one to use
#' is chosen in priority order:
#' \enumerate{
#'   \item The GLM version pinned via the `AEME.glm_version` option (set by
#'   [install_glm_aed()], or by the caller directly).
#'   \item Whichever GLM version is actually installed locally (see
#'   [glm_exe_path()]), checked directly rather than trusting session state.
#'   \item If neither can be determined, the highest version number among
#'   the files present (e.g. `glm4.nml` over `glm3.nml`).
#' }
#' A message reports which file was picked and why, since an unused
#' `glm<version>.nml` sitting in the directory is otherwise easy to miss.
#'
#' @examples
#' glm_dir <- file.path(tempdir(), "glm_aed")
#' dir.create(glm_dir, showWarnings = FALSE)
#' file.create(file.path(glm_dir, "glm3.nml"))
#' find_glm_nml(glm_dir)
#'
#' @export
find_glm_nml <- function(dir, must_exist = TRUE) {
  files <- list.files(dir, pattern = "^glm[0-9]+\\.nml$", full.names = TRUE)
  if (length(files) == 0) {
    if (must_exist) {
      cli::cli_abort(
        "No GLM hydrodynamic nml file (matching {.code glm<version>.nml}) found in {.file {dir}}."
      )
    }
    return(NA_character_)
  }
  if (length(files) == 1) {
    return(files)
  }
  files[.pick_glm_version(basename(files))]
}

#' Identify which element of a named vector/list is the GLM hydrodynamic nml
#'
#' Companion to `find_glm_nml()` for code that already has a named
#' vector/list of discovered config files (keyed by basename without
#' extension, e.g. `"glm3"`, `"glm4"`, `"aed"`) rather than a directory to
#' search, such as the output of [get_model_config_files()]. When more than
#' one `glm<version>` entry is present, resolved the same way as
#' [find_glm_nml()] -- see its Details.
#'
#' @param names_vec character; names to search, e.g. `names(cfg)`.
#' @param must_exist logical; abort if none is found. Default `TRUE`.
#'
#' @return character(1); the matching name, or `NA_character_` if
#'   `must_exist = FALSE` and none was found.
#' @noRd
find_glm_nml_key <- function(names_vec, must_exist = TRUE) {
  matches <- names_vec[grepl("^glm[0-9]+$", names_vec)]
  if (length(matches) == 0) {
    if (must_exist) {
      cli::cli_abort(
        "No GLM hydrodynamic nml entry (matching {.code glm<version>}) found."
      )
    }
    return(NA_character_)
  }
  if (length(matches) == 1) {
    return(matches)
  }
  matches[.pick_glm_version(matches)]
}

#' Pick which of several `glm<version>` candidates to use
#'
#' @param candidates character vector (length >= 2), each containing a
#'   `glm<version>` substring -- either bare keys (`"glm3"`, `"glm4"`) or
#'   full filenames/paths (`".../glm4.nml"`).
#' @return integer; the index of the chosen candidate.
#' @noRd
.pick_glm_version <- function(candidates) {
  versions <- as.integer(gsub(".*glm([0-9]+).*", "\\1", candidates))
  preferred <- .preferred_glm_major_version()

  idx <- if (!is.null(preferred) && preferred %in% versions) {
    which(versions == preferred)[1]
  } else {
    which.max(versions)
  }

  cli::cli_inform(c(
    "i" = "Found multiple GLM hydrodynamic nml candidates: {.val {candidates}}.",
    "v" = "Using {.val {candidates[idx]}}{if (is.null(preferred)) ' (highest GLM version found)' else ' (matches the installed/pinned GLM version)'}."
  ))
  idx
}

#' Determine the GLM major version to prefer when more than one
#' `glm<version>.nml` candidate is present
#'
#' Mirrors the priority order .resolve_glm_exec() itself uses to pick a
#' GLM binary, minus the `AEME.glm_exec` raw-path override (which would
#' require actually running the executable to learn its version, too heavy
#' for a plain file-discovery helper):
#' \enumerate{
#'   \item The `AEME.glm_version` option, if set.
#'   \item Whichever GLM version is installed locally on disk (authoritative
#'   regardless of session state).
#' }
#' @return integer major version (e.g. `3L`), or `NULL` if neither could be
#'   determined.
#' @noRd
.preferred_glm_major_version <- function() {
  pinned <- getOption("AEME.glm_version", default = NULL)
  if (!is.null(pinned)) {
    major <- .glm_major_version(pinned)
    if (!is.null(major)) return(major)
  }

  installed <- tryCatch(.glm_latest_installed_version(), error = function(e) NULL)
  if (!is.null(installed)) {
    major <- .glm_major_version(installed)
    if (!is.null(major)) return(major)
  }

  NULL
}

#' Extract the major version number from a GLM version string
#' @param x character; e.g. `"3.9.108"`.
#' @return integer, e.g. `3L`, or `NULL` if unparseable.
#' @noRd
.glm_major_version <- function(x) {
  major <- suppressWarnings(as.integer(sub("^([0-9]+)\\..*$", "\\1", x)))
  if (length(major) != 1 || is.na(major)) NULL else major
}
