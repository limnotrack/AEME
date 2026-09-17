#' Curated map of editable DYRESM-CAEDYM parameters
#'
#' Names a user can pass to [set_dy_cd_param()] / [get_dy_cd_param()],
#' mapped to the config file they live in (`"cfg"` -> `<lakename>.cfg`,
#' `"par"` -> `dyresm3p1.par`) and a fixed substring of the trailing
#' `#` comment that uniquely identifies the line. DYRESM-CAEDYM's config
#' files are positional plain text with no key = value structure, so this
#' table is what makes an `aeme`-free "set a parameter by name" workflow
#' possible at all.
#'
#' @format named list; each element `list(file, tag)`.
#' @noRd
.dy_cd_param_map <- list(
  # --- <lakename>.cfg ---
  start_date          = list(file = "cfg", tag = "# start date"),
  sim_days            = list(file = "cfg", tag = "simulation length"),
  run_caedym          = list(file = "cfg", tag = "run CAEDYM"),
  output_interval     = list(file = "cfg", tag = "output Interval"),
  Kw                  = list(file = "cfg", tag = "light extinction coefficient"),
  min_layer_thickness = list(file = "cfg", tag = "minimum layer thickness"),
  max_layer_thickness = list(file = "cfg", tag = "maximum layer thickness"),
  timestep            = list(file = "cfg", tag = "time Step"),
  # --- dyresm3p1.par ---
  drag_coef            = list(file = "par", tag = "bulk aerodynamic"),
  albedo               = list(file = "par", tag = "mean albedo"),
  emissivity           = list(file = "par", tag = "emissivity of a water surface"),
  crit_wind_speed      = list(file = "par", tag = "critical wind speed"),
  output_time          = list(file = "par", tag = "time of day for output"),
  bubbler_entrain_coef = list(file = "par", tag = "bubbler entrainment coefficient"),
  plume_entrain_coef   = list(file = "par", tag = "buoyant plume entrainment"),
  eta_K                = list(file = "par", tag = "shear production efficiency"),
  eta_P                = list(file = "par", tag = "potential energy mixing efficiency"),
  eta_S                = list(file = "par", tag = "wind stirring efficiency"),
  eff_surf_area_coef   = list(file = "par", tag = "effective surface area coeff"),
  bbl_dissip_coef      = list(file = "par", tag = "bbl dissipation coeff"),
  vert_mix_coef        = list(file = "par", tag = "vertical mixing coeff")
)

#' Locate the single value line in a positional DYRESM-CAEDYM config file
#'
#' @param lines character; the file, one element per line.
#' @param tag character; a fixed substring of the target line's trailing
#'   `#` comment.
#' @return integer(1); the matching line index.
#' @noRd
.dy_cd_find_line <- function(lines, tag) {
  has_hash <- grepl("#", lines, fixed = TRUE)
  hits <- which(has_hash & grepl(tag, lines, fixed = TRUE))
  # Drop title/banner lines that carry no value before the '#'
  hits <- hits[nchar(trimws(sub("#.*$", "", lines[hits]))) > 0]
  if (length(hits) == 0) {
    cli::cli_abort("Could not find a parameter line matching {.val {tag}}.")
  }
  hits[1]
}

#' Read the leading value token from a positional config line
#' @noRd
.dy_cd_get_value <- function(line) {
  pre <- sub("#.*$", "", line)
  tok <- trimws(pre)
  num <- suppressWarnings(as.numeric(tok))
  if (is.na(num)) tok else num
}

#' Replace the leading value token on a positional config line, preserving
#' the leading indent and the column the `#` comment starts in
#' @noRd
.dy_cd_set_value <- function(line, value) {
  hash <- regexpr("#", line, fixed = TRUE)
  pre <- substr(line, 1, hash - 1)
  post <- substr(line, hash, nchar(line))
  lead <- sub("^([[:space:]]*).*$", "\\1", pre)
  val_txt <- if (is.numeric(value)) {
    formatC(value, digits = 6, format = "g")
  } else {
    as.character(value)
  }
  new_pre <- paste0(lead, val_txt)
  if (nchar(new_pre) < nchar(pre)) {
    new_pre <- paste0(new_pre, strrep(" ", nchar(pre) - nchar(new_pre)))
  } else {
    new_pre <- paste0(new_pre, "  ")
  }
  paste0(new_pre, post)
}

#' Set one or more parameter values in a DYRESM-CAEDYM configuration
#'
#' Thin, `aeme`-free wrapper for editing a DYRESM-CAEDYM model directory in
#' place. DYRESM-CAEDYM has no keyed config file like GLM-AED's `.nml` --
#' its tunables are split between the positional `<lakename>.cfg`
#' (light extinction, layer thickness limits, time step, output interval)
#' and the positional `dyresm3p1.par` (bulk aerodynamic drag, albedo,
#' emissivity, mixing efficiencies, ...). This function exposes a curated
#' set of those by friendly name and edits the right line of the right
#' file, leaving formatting otherwise untouched. Intended for a
#' DYRESM-CAEDYM-only workflow where a user just wants to tweak parameters,
#' run the model, and load the output.
#'
#' Setting `Kw` also updates the `PAR` line of `caedym3p1.bio` when that
#' file is present, mirroring [build_aeme()]'s own behaviour so a BGC run
#' stays consistent.
#'
#' Accepted names:
#' \describe{
#'   \item{`<lakename>.cfg`}{`start_date`, `sim_days`, `run_caedym`,
#'   `output_interval`, `Kw`, `min_layer_thickness`, `max_layer_thickness`,
#'   `timestep`}
#'   \item{`dyresm3p1.par`}{`drag_coef`, `albedo`, `emissivity`,
#'   `crit_wind_speed`, `output_time`, `bubbler_entrain_coef`,
#'   `plume_entrain_coef`, `eta_K`, `eta_P`, `eta_S`, `eff_surf_area_coef`,
#'   `bbl_dissip_coef`, `vert_mix_coef`}
#' }
#'
#' @param path_dy filepath; directory containing the DYRESM-CAEDYM
#' configuration (the `dy_cd` model directory).
#' @param ... named parameter/value pairs to set, e.g. `Kw = 0.5`,
#' `max_layer_thickness = 2`, `eta_S = 0.5`. See Details for accepted
#' names.
#' @param cfg_file filepath; the `<lakename>.cfg` file to edit. Defaults to
#' the one found in `path_dy` via [find_dy_cd_cfg()].
#' @param par_file filepath; the `dyresm3p1.par` file to edit. Defaults to
#' `dyresm3p1.par` in `path_dy`.
#'
#' @return invisibly, a named list of the values that were set.
#' @export
#'
#' @examples
#' \dontrun{
#' set_dy_cd_param(path_dy, Kw = 0.5, max_layer_thickness = 2)
#' set_dy_cd_param(path_dy, eta_K = 0.1, eta_P = 0.3, eta_S = 0.5)
#' }
set_dy_cd_param <- function(path_dy, ...,
                            cfg_file = find_dy_cd_cfg(path_dy),
                            par_file = file.path(path_dy, "dyresm3p1.par")) {

  arg_list <- list(...)
  if (length(arg_list) == 0) {
    cli::cli_abort("Provide at least one name = value pair to set.")
  }
  if (is.null(names(arg_list)) || any(names(arg_list) == "")) {
    cli::cli_abort("All arguments in '...' must be named, e.g. Kw = 0.5.")
  }
  unknown <- setdiff(names(arg_list), names(.dy_cd_param_map))
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "Unknown DYRESM-CAEDYM parameter{?s}: {.val {unknown}}.",
      "i" = "Accepted: {.val {names(.dy_cd_param_map)}}"
    ))
  }

  files <- c(cfg = cfg_file, par = par_file)
  by_file <- split(names(arg_list),
                   vapply(names(arg_list), \(n) .dy_cd_param_map[[n]]$file,
                          character(1)))

  for (fkey in names(by_file)) {
    f <- files[[fkey]]
    if (!file.exists(f)) {
      cli::cli_abort("Config file not found: {.file {f}}")
    }
    lines <- readLines(f, warn = FALSE)
    for (nm in by_file[[fkey]]) {
      idx <- .dy_cd_find_line(lines, .dy_cd_param_map[[nm]]$tag)
      lines[idx] <- .dy_cd_set_value(lines[idx], arg_list[[nm]])
    }
    writeLines(lines, f)
  }

  # Keep caedym3p1.bio's PAR line in step with Kw, as make_dy_cd_cfg() does.
  if ("Kw" %in% names(arg_list)) {
    bio_file <- file.path(dirname(cfg_file), "caedym3p1.bio")
    if (file.exists(bio_file)) {
      txt <- readLines(bio_file, warn = FALSE)
      sel <- grep("PAR,", txt)
      if (length(sel) == 1) {
        txt[sel] <- paste0("     ",
                           formatC(arg_list[["Kw"]], digits = 5, format = "f"),
                           "            0.450               : PAR, Photosynthetically Active")
        writeLines(txt, bio_file)
      }
    }
  }

  invisible(arg_list)
}

#' Get one or more parameter values from a DYRESM-CAEDYM configuration
#'
#' Companion to [set_dy_cd_param()] for reading current values without an
#' `aeme` object.
#'
#' @inheritParams set_dy_cd_param
#' @param name character vector; name(s) of the parameter(s) to read -- the
#' same friendly names [set_dy_cd_param()] accepts.
#'
#' @return the parameter value if `name` has length 1, otherwise a named
#' list of values. Numeric-looking values are returned as numbers, others
#' (e.g. `start_date`, `run_caedym`'s `.TRUE.`/`.FALSE.`) as strings.
#' @export
#'
#' @examples
#' \dontrun{
#' get_dy_cd_param(path_dy, "Kw")
#' get_dy_cd_param(path_dy, c("Kw", "max_layer_thickness", "eta_S"))
#' }
get_dy_cd_param <- function(path_dy, name,
                            cfg_file = find_dy_cd_cfg(path_dy),
                            par_file = file.path(path_dy, "dyresm3p1.par")) {

  unknown <- setdiff(name, names(.dy_cd_param_map))
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "Unknown DYRESM-CAEDYM parameter{?s}: {.val {unknown}}.",
      "i" = "Accepted: {.val {names(.dy_cd_param_map)}}"
    ))
  }

  files <- c(cfg = cfg_file, par = par_file)
  cache <- list()

  vals <- lapply(name, \(n) {
    fkey <- .dy_cd_param_map[[n]]$file
    if (is.null(cache[[fkey]])) {
      f <- files[[fkey]]
      if (!file.exists(f)) cli::cli_abort("Config file not found: {.file {f}}")
      cache[[fkey]] <<- readLines(f, warn = FALSE)
    }
    idx <- .dy_cd_find_line(cache[[fkey]], .dy_cd_param_map[[n]]$tag)
    .dy_cd_get_value(cache[[fkey]][idx])
  })
  names(vals) <- name

  if (length(name) == 1) {
    return(vals[[1]])
  }
  vals
}
