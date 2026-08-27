#' Set initial conditions for a DYRESM-CAEDYM simulation
#'
#' Thin, `aeme`-free wrapper for editing the initial temperature/salinity
#' profile (`<lakename>.pro`) and, for a CAEDYM (BGC) run, the water-quality
#' initial values (`<lakename>.int`) of a DYRESM-CAEDYM model directory in
#' place. Intended for a DYRESM-CAEDYM-only workflow where a user just wants
#' to tweak initial conditions, run the model, and load the output.
#'
#' Existing profile depths in `<lakename>.pro` are left unchanged --
#' `temp`/`salt` values are recycled (via [rep_len()]) across however many
#' rows are already defined. `wq_init` overwrites the water-column initial
#' value for the named CAEDYM variables in `<lakename>.int`, leaving their
#' sediment initial values (and every other variable) untouched.
#'
#' @param path_dy filepath; directory containing the DYRESM-CAEDYM
#' configuration (the `dy_cd` model directory).
#' @param temp numeric; new initial water temperature profile. Recycled to
#' the number of rows in `<lakename>.pro`. `NULL` (default) leaves it
#' unchanged.
#' @param salt numeric; new initial salinity profile, same recycling rule
#' as `temp`. `NULL` (default) leaves it unchanged.
#' @param wq_init named list; new water-column initial values for CAEDYM
#' water quality variables, keyed by `var_aeme` name, e.g.
#' `list(CHM_oxy = 300, NIT_amm = 0.5)`. Names are translated to CAEDYM's
#' own variable names via `rename_modelvars()`. Each value is a single
#' number (CAEDYM `.int` water-column initials are not depth-resolved).
#' `NULL` (default) leaves water quality initial values unchanged. Requires
#' a `<lakename>.int` file (a CAEDYM configuration).
#' @param pro_file,int_file filepath; the `.pro` / `.int` files to edit.
#' Default to the files found in `path_dy` via [find_dy_cd_cfg()]'s prefix.
#'
#' @return invisibly, the updated initial-profile data.frame
#' (`depth`/`temp`/`salt`).
#' @export
#'
#' @examples
#' \dontrun{
#' set_dy_cd_init(path_dy, temp = seq(20, 10, length.out = 10))
#' set_dy_cd_init(path_dy, wq_init = list(CHM_oxy = 300, NIT_amm = 0.5))
#' }
set_dy_cd_init <- function(path_dy, temp = NULL, salt = NULL, wq_init = NULL,
                           pro_file = NULL, int_file = NULL) {

  if (is.null(temp) && is.null(salt) && is.null(wq_init)) {
    cli::cli_abort("Provide at least one of 'temp', 'salt' or 'wq_init'.")
  }

  prefix <- .dy_cd_prefix(path_dy)
  if (is.null(pro_file)) pro_file <- file.path(path_dy, paste0(prefix, ".pro"))
  if (is.null(int_file)) int_file <- file.path(path_dy, paste0(prefix, ".int"))

  prof <- .read_dy_pro(pro_file)

  if (!is.null(temp) || !is.null(salt)) {
    if (!is.null(temp)) prof$temp <- rep_len(temp, nrow(prof))
    if (!is.null(salt)) prof$salt <- rep_len(salt, nrow(prof))
    .write_dy_pro(pro_file, prof)
  }

  if (!is.null(wq_init) && length(wq_init) > 0) {
    .set_dy_cd_wq_init(int_file = int_file, wq_init = wq_init)
  }

  invisible(prof)
}

#' Read a DYRESM-CAEDYM `<lakename>.pro` initial-profile file
#'
#' @param file filepath; to a `.pro` file.
#' @return data.frame with `depth`, `temp`, `salt` columns.
#' @noRd
.read_dy_pro <- function(file) {
  if (!file.exists(file)) {
    cli::cli_abort("Initial profile file not found: {.file {file}}")
  }
  lines <- readLines(file, warn = FALSE)
  hdr <- grep("T\\(degC\\)|Elev\\(m\\)", lines)
  if (length(hdr) == 0) {
    cli::cli_abort("Could not find the column header row in {.file {file}}.")
  }
  hdr <- hdr[1]
  data_lines <- lines[(hdr + 1):length(lines)]
  data_lines <- data_lines[nchar(trimws(data_lines)) > 0]
  m <- utils::read.table(text = paste(data_lines, collapse = "\n"),
                         header = FALSE)
  if (ncol(m) < 3) {
    cli::cli_abort("Expected 3 columns (depth, temp, salt) in {.file {file}}.")
  }
  data.frame(depth = m[[1]], temp = m[[2]], salt = m[[3]])
}

#' Write a DYRESM-CAEDYM `<lakename>.pro` file, keeping its header lines
#'
#' @param file filepath; to a `.pro` file that already exists (its header
#'   lines up to and including the `Elev(m) ...` row are preserved).
#' @param prof data.frame with `depth`, `temp`, `salt` columns.
#' @return invisibly, `NULL`.
#' @noRd
.write_dy_pro <- function(file, prof) {
  lines <- readLines(file, warn = FALSE)
  hdr <- grep("T\\(degC\\)|Elev\\(m\\)", lines)[1]
  head_lines <- lines[seq_len(hdr)]
  # Update the "<n>   # initial profile n layers" count if present
  n_idx <- grep("n layers", head_lines)
  if (length(n_idx) == 1) {
    head_lines[n_idx] <- sub("^\\s*[0-9]+", nrow(prof), head_lines[n_idx])
  }
  body <- paste(format(prof$depth), format(prof$temp), format(prof$salt),
                sep = "\t")
  writeLines(c(head_lines, body), file)
  invisible(NULL)
}

#' Overwrite water-column initial values in a DYRESM-CAEDYM `<lakename>.int`
#'
#' @param int_file filepath; to a `.int` file.
#' @param wq_init named list; keyed by `var_aeme` name, one value each.
#' @return invisibly, `NULL`.
#' @noRd
.set_dy_cd_wq_init <- function(int_file, wq_init) {
  if (!file.exists(int_file)) {
    cli::cli_abort(c(
      "CAEDYM initial-conditions file not found: {.file {int_file}}",
      "i" = "'wq_init' requires a CAEDYM (BGC) configuration."
    ))
  }
  lines <- readLines(int_file, warn = FALSE)

  var_names <- names(wq_init)
  cd_names <- rename_modelvars(var_names, type_output = "dy_cd",
                               warn_unmatched = TRUE)

  start <- grep("^3D DATA", lines)
  stop <- grep("^2D DATA", lines)
  if (length(start) != 1 || length(stop) != 1 || stop <= start) {
    cli::cli_abort("Unexpected structure in {.file {int_file}} (no 3D DATA / 2D DATA block).")
  }

  for (i in seq_along(var_names)) {
    cd <- cd_names[i]
    if (is.na(cd) || cd == "") next
    # Each block: VAR / "  CO_I" / "  <wc>" / "  <sed>"
    var_idx <- which(trimws(lines) == cd & seq_along(lines) > start &
                       seq_along(lines) < stop)
    if (length(var_idx) == 0) {
      cli::cli_warn("'{var_names[i]}' ({cd}) not found in {.file {int_file}} -- skipped.")
      next
    }
    wc_idx <- var_idx[1] + 2
    lead <- sub("^([[:space:]]*).*$", "\\1", lines[wc_idx])
    lines[wc_idx] <- paste0(lead, wq_init[[var_names[i]]])
  }

  writeLines(lines, int_file)
  invisible(NULL)
}
