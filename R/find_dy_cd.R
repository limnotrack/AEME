#' Detect the DYRESM-CAEDYM configuration file within a directory
#'
#' A DYRESM-CAEDYM model directory holds a family of files that all share a
#' single `<lakename>` prefix (`<lakename>.cfg`, `.con`, `.stg`, `.inf`,
#' `.wdr`, `.met`, `.pro`, `.int`). The `<lakename>` is not knowable from
#' the directory name alone, so these helpers recover it from the `.stg`
#' file that is always present (the same approach [run_dy_cd()] itself
#' uses), and return the full path to the requested companion file.
#'
#' `find_dy_cd_cfg()` returns the `.cfg` file (DYRESM-CAEDYM's top-level
#' configuration), analogous to [find_glm_nml()] for GLM-AED.
#'
#' @param dir character; the `dy_cd` model directory (not searched
#'   recursively).
#' @param must_exist logical; abort if no `.stg` file is found. Default
#'   `TRUE`.
#'
#' @return character; full path to the matching file, or `NA_character_` if
#'   `must_exist = FALSE` and no `.stg` file was found.
#'
#' @examples
#' \dontrun{
#' find_dy_cd_cfg(path_dy)
#' }
#'
#' @export
find_dy_cd_cfg <- function(dir, must_exist = TRUE) {
  prefix <- .dy_cd_prefix(dir, must_exist = must_exist)
  if (is.na(prefix)) {
    return(NA_character_)
  }
  file.path(dir, paste0(prefix, ".cfg"))
}

#' Recover the `<lakename>` prefix shared by a DYRESM-CAEDYM config directory
#'
#' @param dir character; the `dy_cd` model directory.
#' @param must_exist logical; abort if no `.stg` file is found.
#'
#' @return character(1); the prefix (basename of the `.stg` file without its
#'   extension), or `NA_character_` if `must_exist = FALSE` and none found.
#' @noRd
.dy_cd_prefix <- function(dir, must_exist = TRUE) {
  stg <- list.files(dir, pattern = "\\.stg$", full.names = FALSE)
  if (length(stg) == 0) {
    if (must_exist) {
      cli::cli_abort(
        "No DYRESM-CAEDYM {.code .stg} file found in {.file {dir}}."
      )
    }
    return(NA_character_)
  }
  if (length(stg) > 1) {
    cli::cli_abort(c(
      "More than one {.code .stg} file found in {.file {dir}}: {.val {stg}}.",
      "i" = "A DYRESM-CAEDYM directory should contain exactly one."
    ))
  }
  sub("\\.stg$", "", stg)
}
