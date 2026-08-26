#' Read a single Simstrat `<var>_out.dat` output file
#'
#' Simstrat writes one plain-text file per output variable (see
#' `strat_outputfile.f90::open_files()` in the Simstrat source): a header row
#' of `Datetime,<z1>,<z2>,...` -- depths (or, for `_zone_out.dat` files, zone
#' heights) written with Fortran `F12.3`, a single trailing column for
#' surface/whole-lake variables -- followed by one `(F12.4)` day number plus
#' `(ES14.4E3)` values per output time.
#'
#' Everything in the file is numeric, so it is read with a single
#' \code{\link[base]{scan}} call rather than \code{\link[utils]{read.csv}} --
#' substantially faster on the large depth x time files, which matters when a
#' calibration reads the output once per model run (see
#' \code{\link{read_simstrat_dat}}).
#'
#' @param file character; path to a `<var>_out.dat` file.
#' @param skip_rows integer; number of *data* rows (after the header) to skip.
#' Lets a caller read only the part of a file it needs, rather than parsing
#' rows it will immediately discard. Default `0`.
#' @param n_rows integer; number of data rows to read after `skip_rows`, or
#' `-1` (default) for all remaining rows.
#'
#' @return List with elements
#' \describe{
#'   \item{`day`}{numeric vector of Simstrat day numbers, one per row read.}
#'   \item{`depths`}{numeric vector of the header's depths/zone heights, one
#'     per value column (length 1 for a surface/whole-lake variable). As
#'     written by Simstrat: negative-down offsets from the lake surface for
#'     water-column variables, positive heights above the lake bottom for
#'     `_zone` variables.}
#'   \item{`values`}{numeric matrix, `length(day)` rows x `length(depths)`
#'     columns, in the file's own column order.}
#'   \item{`offset`}{integer; `skip_rows`, so a caller can map row indices in
#'     the full file onto rows of `values`.}
#' }
#'
#' @export
read_simstrat_dat_file <- function(file, skip_rows = 0L, n_rows = -1L) {

  if (!file.exists(file)) {
    cli::cli_abort(c("x" = "Simstrat output file {.path {file}} not found."))
  }

  header <- scan(file, what = "", sep = ",", nlines = 1L, quiet = TRUE)
  n_col <- length(header)
  if (n_col < 2) {
    cli::cli_abort(c("x" = "Simstrat output file {.path {file}} has no value
                     columns."))
  }
  depths <- suppressWarnings(as.numeric(header[-1]))

  vals <- tryCatch(
    scan(file, what = numeric(), sep = ",", skip = 1L + as.integer(skip_rows),
         nlines = as.integer(n_rows), quiet = TRUE,
         na.strings = c("NA", "NaN", "nan", "NAN")),
    error = function(e) {
      # Fortran can emit tokens R's numeric parser rejects outright (e.g.
      # `**********` when a value overflows its ES14.4E3 field, or a platform
      # spelling of NaN/Inf). Re-read as text and coerce, so one bad value
      # becomes an NA rather than an unreadable file.
      txt <- scan(file, what = "", sep = ",", skip = 1L + as.integer(skip_rows),
                  nlines = as.integer(n_rows), quiet = TRUE)
      out <- suppressWarnings(as.numeric(txt))
      n_bad <- sum(is.na(out) & !txt %in% c("NA", "NaN", "nan", "NAN"))
      if (n_bad > 0) {
        cli::cli_warn(c("!" = "{n_bad} unparseable value{?s} in
                        {.path {file}} read as {.val NA}."))
      }
      out
    }
  )

  n_row <- length(vals) %/% n_col
  if (length(vals) %% n_col != 0) {
    # A partially-written final row -- the run was killed, or ran out of disk,
    # part way through writing that timestep. Drop it rather than abort, so
    # what did complete is still readable.
    cli::cli_warn(c("!" = "Incomplete final row in {.path {file}} dropped."))
    vals <- vals[seq_len(n_row * n_col)]
  }
  mat <- matrix(vals, ncol = n_col, byrow = TRUE)

  list(day = mat[, 1], depths = depths,
       values = mat[, -1, drop = FALSE], offset = as.integer(skip_rows))
}

#' Read a Simstrat `<var>_out.dat` file's header only
#'
#' Used to learn a variable's column count/depths without parsing its data
#' (see \code{\link{read_simstrat_dat}}, which needs both to pick the shared
#' depth grid and to tell surface variables from depth profiles).
#'
#' @param file character; path to a `<var>_out.dat` file.
#' @return numeric vector of the header's depths/zone heights, or `NULL` if
#'   the file could not be read.
#' @noRd
.read_simstrat_dat_header <- function(file) {
  header <- tryCatch(scan(file, what = "", sep = ",", nlines = 1L,
                          quiet = TRUE),
                     error = function(e) NULL)
  if (length(header) < 2) return(NULL)
  suppressWarnings(as.numeric(header[-1]))
}

#' Resolve a Simstrat simulation's output directory and reference year
#'
#' @param sim_folder character; the simulation directory.
#' @param config_file character; `simstrat.par`, either a full path or a name
#'   relative to `sim_folder`.
#' @return list with elements `out_dir`, `ref_year` and `config_file`.
#' @noRd
.simstrat_par_paths <- function(sim_folder, config_file = "simstrat.par") {
  if (!file.exists(config_file)) {
    # Try to find it in the sim_folder if not given as a full path
    config_file <- file.path(sim_folder, config_file)
    if (!file.exists(config_file)) {
      cli::cli_abort(c("x" = "Simstrat configuration file {.file simstrat.par}
                       not found in {.path {sim_folder}}."))
    }
  }
  par <- jsonlite::fromJSON(config_file, simplifyVector = FALSE)
  list(out_dir = file.path(sim_folder, par[["Output"]][["Path"]]),
       ref_year = as.integer(par[["Simulation"]][["Reference year"]]),
       config_file = config_file)
}

#' `units`/`long_name` lookup for native Simstrat-AED2/AED variable names
#'
#' Both Simstrat couplings' native names live in their own `key_naming`
#' column, and a variable can appear in either. Shared by
#' \code{\link{write_simstrat_nc}} (which writes these as netCDF attributes)
#' and \code{\link{read_simstrat_dat}} (which has no netCDF attributes to read
#' back, so labels raw output from the same source).
#'
#' @return list with elements `units` and `long_name`, each a character vector
#'   named by native Simstrat variable name.
#' @noRd
.simstrat_var_meta <- function() {
  data("key_naming", package = "AEME", envir = environment())
  lookup <- rbind(
    stats::setNames(key_naming[nzchar(key_naming$simstrat_aed2),
                               c("simstrat_aed2", "units", "name_text")],
                    c("native", "units", "long_name")),
    stats::setNames(key_naming[nzchar(key_naming$simstrat_aed),
                               c("simstrat_aed", "units", "name_text")],
                    c("native", "units", "long_name"))
  )
  lookup <- lookup[!duplicated(lookup$native), ]
  list(units = stats::setNames(lookup$units, lookup$native),
       long_name = stats::setNames(lookup$long_name, lookup$native))
}

#' Read Simstrat's raw text output directly
#'
#' Reads Simstrat's own `<var>_out.dat` text files straight from the
#' simulation's output directory, returning the same output list as
#' \code{\link{read_simstrat_output}} does from the consolidated `output.nc`
#' -- same keys, same depth grid, same unit conversions -- without going
#' through netCDF at all.
#'
#' This is the fast path for workflows that read model output once per model
#' run, such as `aemetools`' calibration and sensitivity analyses. Reading the
#' text directly avoids both the netCDF write (which serialises *every*
#' output variable, compressed) and the netCDF read, and only the files
#' actually asked for are parsed: with `load_all = FALSE` (the default here,
#' unlike \code{\link{read_simstrat_output}}) a single-variable read touches
#' one `<var>_out.dat` file plus the small `WaterH_out.dat`, however many
#' variables the run wrote. When `date_index`/`dates` select a window of the
#' simulation, only rows up to the end of that window are parsed.
#'
#' Simstrat writes every water-column variable on the same output depth grid
#' (`Output.Depths` in `simstrat.par`), so -- exactly as in the netCDF the
#' `.dat` files are otherwise converted to -- depth profiles share one `z`
#' grid, taken from the first depth-varying file. AED sediment-zone output
#' (`<var>_zone_out.dat`) is on its own zone axis instead, and is returned as
#' an \code{\link{new_grouped_var}} object, again matching
#' \code{\link{read_simstrat_output}}.
#'
#' Note that this needs the `.dat` files to still be there: they are kept by
#' default, but \code{\link{write_simstrat_nc}}`(remove_dat = TRUE)` deletes
#' them once they have been written to `output.nc`.
#'
#' @inheritParams read_simstrat_output
#' @param sim_folder character; path to the `simstrat_aed2`/`simstrat_aed`
#' simulation directory (containing `simstrat.par` and the output directory
#' it points at). Not needed if both `out_dir` and `ref_year` are supplied.
#' @param load_all logical; also load every other variable present in the
#' output directory beyond the declared `vars_sim` set. Default `FALSE` --
#' the opposite of \code{\link{read_simstrat_output}}'s default, because
#' loading every variable means reading every file, which is the cost this
#' function exists to avoid.
#' @param config_file character; name of (or path to) the Simstrat
#' configuration file. Default `"simstrat.par"`.
#' @param out_dir character; the output directory itself, skipping the
#' `simstrat.par` lookup. Default `NULL` (read from `simstrat.par`).
#' @param ref_year integer; the simulation's `Simulation.Reference year`,
#' skipping the `simstrat.par` lookup. Default `NULL` (read from
#' `simstrat.par`). Supply both this and `out_dir` in a tight loop to avoid
#' re-parsing the configuration on every read.
#'
#' @returns List with AEME output variables, classed as for
#' \code{\link{read_simstrat_output}}, or a `model_output_error` (see
#' \code{\link{is_model_error}}) if the output directory holds no readable
#' output.
#'
#' @seealso \code{\link{read_simstrat_output}} to read the same output back
#' from `output.nc`, \code{\link{write_simstrat_nc}} to produce that file,
#' and \code{\link{read_simstrat_dat_file}} for the single-file primitive.
#'
#' @export
#'
#' @importFrom withr local_locale local_timezone
#' @importFrom dplyr filter mutate pull left_join rename
read_simstrat_dat <- function(sim_folder = NULL, vars_sim = NULL,
                              depths = NULL, dates = NULL, date_index = NULL,
                              incl_fluxes = TRUE, load_all = FALSE,
                              raw_output = FALSE, model = "simstrat_aed2",
                              config_file = "simstrat.par", out_dir = NULL,
                              ref_year = NULL) {

  if (isTRUE(raw_output) && !is.null(depths)) {
    cli::cli_abort("'depths' cannot be supplied when 'raw_output = TRUE' -- raw output uses Simstrat's native output depths.")
  }

  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")

  if (is.null(out_dir) || is.null(ref_year)) {
    par_info <- .simstrat_par_paths(sim_folder = sim_folder,
                                    config_file = config_file)
    if (is.null(out_dir)) out_dir <- par_info$out_dir
    if (is.null(ref_year)) ref_year <- par_info$ref_year
  }

  dat_files <- list.files(out_dir, pattern = "_out\\.dat$", full.names = TRUE)
  if (length(dat_files) == 0) {
    return(empty_model_output(
      reason = paste0("No Simstrat *_out.dat output files found in ", out_dir)
    ))
  }
  names(dat_files) <- gsub("_out\\.dat$", "", basename(dat_files))
  all_vars <- names(dat_files)
  is_zone <- stats::setNames(grepl("_zone$", all_vars), all_vars)

  # ---- Which files does this call actually need to read? ----
  model_vars_vec <- NULL
  if (!is.null(vars_sim)) {
    model_vars <- get_model_vars(vars_sim = vars_sim, model = model)
    model_vars_vec <- get_model_vars(vars_sim = vars_sim, model = model,
                                     as_vector = TRUE)
    vars_chk <- data.frame(
      vars = model_vars_vec,
      present = model_vars_vec %in% all_vars
    ) |>
      dplyr::left_join(model_vars, by = stats::setNames(model, "vars")) |>
      dplyr::rename(conv_factor = conversion_aed)
  }

  flux_vars <- if (incl_fluxes) c("HK", "HV", "Rad0", "T") else character()
  needed <- unique(c("WaterH", flux_vars, unname(model_vars_vec)))
  if (isTRUE(load_all)) needed <- all_vars
  needed <- intersect(needed, all_vars)
  if (length(needed) == 0) {
    # None of what was asked for -- nor the water level/flux variables -- is
    # in this output. The simulation times and depth grid still are, and are
    # still worth returning, so sweep every file's header for them instead of
    # bailing out.
    needed <- all_vars
  }

  # Headers are cheap (one line each) and give each variable's column count --
  # what distinguishes a surface variable from a depth profile -- as well as
  # the shared z grid.
  headers <- lapply(dat_files[needed], .read_simstrat_dat_header)
  names(headers) <- needed

  # ---- Simulation times ----
  # Every output file shares the same output times, so read them from the
  # cheapest available file: a surface variable (a single value column) if
  # there is one, otherwise the smallest file on disk.
  n_col <- vapply(headers, \(h) if (is.null(h)) NA_integer_ else length(h),
                  integer(1))
  surface_vars <- needed[!is.na(n_col) & n_col == 1]
  time_var <- if ("WaterH" %in% surface_vars) {
    "WaterH"
  } else if (length(surface_vars) > 0) {
    surface_vars[1]
  } else {
    readable <- needed[!is.na(n_col)]
    if (length(readable) == 0) {
      return(empty_model_output(
        reason = paste0("No readable Simstrat output files in ", out_dir)
      ))
    }
    readable[which.min(file.size(dat_files[readable]))]
  }

  # Cache reads, so a variable that is both a flux/level and a requested
  # variable is only parsed once. Entries record the row offset they were
  # read at (see row_skip below), so global date indices can be mapped onto
  # them.
  cache <- new.env(parent = emptyenv())
  row_skip <- 0L
  row_n <- -1L
  get_dat <- function(v) {
    d <- cache[[v]]
    if (!is.null(d)) return(d)
    d <- read_simstrat_dat_file(dat_files[[v]], skip_rows = row_skip,
                                n_rows = row_n)
    assign(v, d, envir = cache)
    d
  }

  time_dat <- get_dat(time_var)
  if (length(time_dat$day) == 0) {
    return(empty_model_output(reason = "Empty time dimension"))
  }
  simstrat_dates <- simstrat_day_to_date(time_dat$day, ref_year) |> as.Date()

  if (is.null(date_index)) {
    if (!is.null(dates)) {
      date_index <- which(simstrat_dates %in% as.Date(dates))
      if (length(date_index) == 0) {
        cli::cli_abort("No output for {model} at specified dates")
      }
    } else {
      date_index <- seq_along(simstrat_dates)
    }
  }
  if (length(simstrat_dates) < max(date_index)) {
    cli::cli_alert_warning("date_index exceeds available {model} output
                          dates. Returning empty output.")
    return(empty_model_output(
      reason = paste0("date_index exceeds available ", model,
                      " output dates")
    ))
  }
  dates <- simstrat_dates[date_index]

  # Every file still to be read can skip the rows before the requested window
  # -- for a calibration scored over the tail of a simulation, that is most of
  # the file.
  row_skip <- min(date_index) - 1L
  row_n <- if (max(date_index) == length(simstrat_dates)) {
    -1L
  } else {
    max(date_index) - row_skip
  }

  # ---- Shared depth grid ----
  # As in the netCDF the .dat files are otherwise converted to: positive-down
  # depths ascending from the surface, taken from the first depth-varying
  # (non-zone) file. Simstrat writes these as negative-down offsets from the
  # surface (OutputDepthReference = "surface").
  z_depths <- NULL
  for (v in needed[!is_zone[needed] & !is.na(n_col) & n_col > 1]) {
    z_depths <- headers[[v]]
    break
  }
  if (is.null(z_depths)) {
    # Nothing requested is a depth profile -- fall back to any other
    # depth-varying file, reading headers only until one is found.
    for (v in setdiff(all_vars[!is_zone], needed)) {
      h <- .read_simstrat_dat_header(dat_files[[v]])
      if (!is.null(h) && length(h) > 1) {
        z_depths <- h
        break
      }
    }
  }
  z <- if (is.null(z_depths)) 0 else sort(-z_depths)
  midpoints <- matrix(rep(z, length(date_index)), ncol = length(date_index))

  out_list <- list()

  lake_level <- if ("WaterH" %in% needed) {
    d <- get_dat("WaterH")
    as.vector(d$values[date_index - d$offset, 1])
  } else {
    rep(max(z, na.rm = TRUE), length(date_index))
  }
  out_list[["LKE_lvlwtr"]] <- lake_level

  if (is.null(depths)) {
    if (isTRUE(raw_output)) {
      # raw mode: report Simstrat's own native output depths, rather than
      # interpolating onto a shared standardised grid
      out_depths <- round(midpoints, 2)
    } else {
      max_depth <- max(lake_level, na.rm = TRUE)
      data("model_layer_structure", package = "AEME", envir = environment())
      depth_fraction <- model_layer_structure |>
        dplyr::filter(z < max_depth) |>
        dplyr::mutate(deps = z / max_depth) |>
        dplyr::pull(deps) |>
        matrix(ncol = 1)
      depth_mat <- depth_fraction %*% t(lake_level)
      out_depths <- round(depth_mat, 2)
    }
  } else {
    out_depths <- matrix(rep(depths, length(dates)),
                         nrow = length(depths),
                         ncol = length(dates))
  }

  # ---- Per-variable extraction ----
  # Same shapes as read_simstrat_output(): a (time) vector for a surface
  # variable, a (depth x time) matrix -- interpolated onto out_depths unless
  # raw_output -- for a depth profile.
  extract_var <- function(v, conv_factor = 1) {
    d <- get_dat(v)
    idx <- date_index - d$offset
    if (ncol(d$values) == 1) {
      return(as.vector(d$values[idx, 1]) * conv_factor)
    }
    # Reorder columns so they run surface -> bottom, matching z
    ord <- order(-d$depths)
    var <- t(d$values[idx, ord, drop = FALSE]) * conv_factor
    mid <- matrix(rep(sort(-d$depths), length(idx)), ncol = length(idx))
    .glm_depth_profile(var = var, midpoints = mid, out_depths = out_depths,
                       raw_output = raw_output)
  }

  if (incl_fluxes) {
    if ("HK" %in% needed) out_list[["LKE_Qh"]] <- extract_var("HK")
    if ("HV" %in% needed) out_list[["LKE_Qe"]] <- extract_var("HV")
    if ("Rad0" %in% needed) out_list[["LKE_Qsw"]] <- extract_var("Rad0")
    if ("T" %in% needed) {
      d <- get_dat("T")
      # Surface value = the shallowest depth, i.e. the largest (least
      # negative) of Simstrat's negative-down output depths
      out_list[["HYD_surft"]] <- as.vector(
        d$values[date_index - d$offset, which.max(d$depths)]
      )
    }
  }

  out_list <- lapply(out_list, as.vector)
  out_list[["Date"]] <- dates
  out_list[["LKE_depths"]] <- out_depths

  if (!is.null(vars_sim)) {
    out_vars <- lapply(model_vars_vec, \(v) {
      if (isFALSE(vars_chk$present[vars_chk$vars == v])) {
        return(NULL)
      }
      # AED unit-conversion factors are an AEME-specific transform, only
      # applied when standardising output -- raw output stays in Simstrat/
      # AED's own units, matching the .dat files exactly
      if (isTRUE(raw_output)) {
        conv_factor <- 1
      } else {
        conv_factor <- vars_chk$conv_factor[vars_chk$vars == v]
        if (is.na(conv_factor)) conv_factor <- 1
      }
      extract_var(v, conv_factor)
    })

    if (isTRUE(raw_output)) {
      # raw mode: key by the native Simstrat variable name (e.g. "T") instead
      # of the translated AEME var_aeme name (e.g. "HYD_temp")
      names(out_vars) <- unname(model_vars_vec)
    }

    out_list <- c(out_list, out_vars)
  }

  # ---- Load every remaining variable present in the output directory ----
  # As in read_simstrat_output(): variables already handled above are skipped,
  # everything else is keyed by its var_aeme name if key_naming has a
  # translation and by its raw Simstrat name otherwise.
  if (isTRUE(load_all)) {
    already_extracted <- c("WaterH", if (incl_fluxes) c("HK", "HV", "Rad0"),
                           unname(model_vars_vec))

    data("key_naming", package = "AEME", envir = environment())
    sim_to_var_aeme <- stats::setNames(key_naming$var_aeme, key_naming[[model]])

    for (v in setdiff(all_vars, already_extracted)) {
      key <- unname(sim_to_var_aeme[v])
      if (is.na(key) || !nzchar(key)) key <- v
      if (key %in% names(out_list)) next

      if (isTRUE(raw_output)) {
        conv_factor <- 1
      } else {
        conv_idx <- match(v, key_naming[[model]])
        conv_factor <- if (!is.na(conv_idx)) key_naming$conversion_aed[conv_idx] else NA
        if (is.na(conv_factor)) conv_factor <- 1
      }

      result <- tryCatch({
        if (is_zone[[v]]) {
          # AED sediment-zone output: its own zone axis (one column per
          # benthic zone, headed by that zone's reference depth), not the
          # water-column z grid -- kept as a labelled array, exactly as
          # read_simstrat_output() does for the same variables.
          d <- get_dat(v)
          idx <- date_index - d$offset
          ord <- order(d$depths)
          new_grouped_var(
            value = t(d$values[idx, ord, drop = FALSE]),
            dim_names = c("zone", "time"),
            dim_values = list(zone = d$depths[ord], time = dates)
          )
        } else {
          extract_var(v, conv_factor)
        }
      }, error = function(e) {
        cli::cli_warn(c("!" = "Could not read variable {.val {v}} from Simstrat output: {conditionMessage(e)}"))
        NULL
      })

      if (!is.null(result)) {
        out_list[[key]] <- result
      }
    }
  }

  if (isTRUE(raw_output)) {
    # "Date"/"LKE_depths"/"ok"/"reason" are the output list's own structural
    # keys, not plotted variables -- see read_simstrat_output()
    out_names <- setdiff(names(out_list), c("Date", "LKE_depths", "ok", "reason"))
    var_names <- get_model_vars(out_names, model = model, as_vector = TRUE)
    for (i in seq_along(var_names)) {
      if (!is.na(var_names[i]) && nzchar(var_names[i])) {
        names(out_list)[names(out_list) == names(var_names)[i]] <- var_names[i]
      }
    }
  }

  out_list <- c(out_list, list(ok = TRUE, reason = NULL))

  var_units <- var_long_name <- NULL
  if (isTRUE(raw_output)) {
    # The .dat files carry no metadata, so labels come from key_naming -- the
    # same source write_simstrat_nc() writes into the netCDF attributes that
    # read_simstrat_output() reports here.
    raw_vars <- setdiff(names(out_list), c("Date", "LKE_depths", "ok", "reason"))
    meta <- .simstrat_var_meta()
    base_names <- sub("_zone$", "", raw_vars)
    var_units <- unname(meta$units[base_names])
    var_units[is.na(var_units)] <- ""
    var_units <- stats::setNames(var_units, raw_vars)
    var_long_name <- unname(meta$long_name[base_names])
    no_long <- is.na(var_long_name) | !nzchar(var_long_name)
    var_long_name[no_long] <- raw_vars[no_long]
    var_long_name <- stats::setNames(var_long_name, raw_vars)
  }

  return(.new_aeme_output(out_list, model = model, raw = raw_output,
                          var_units = var_units,
                          var_long_name = var_long_name))
}

#' Read Simstrat lake water level from the raw text output
#'
#' The \code{\link{read_simstrat_wlev}} equivalent for Simstrat's own
#' `WaterH_out.dat`, for callers that have not written (or have deleted) the
#' consolidated `output.nc`.
#'
#' @inheritParams read_simstrat_dat
#' @returns Data frame with Date and LKE_lvlwtr columns.
#' @export
read_simstrat_dat_wlev <- function(sim_folder = NULL,
                                   config_file = "simstrat.par",
                                   out_dir = NULL, ref_year = NULL) {
  if (is.null(out_dir) || is.null(ref_year)) {
    par_info <- .simstrat_par_paths(sim_folder = sim_folder,
                                    config_file = config_file)
    if (is.null(out_dir)) out_dir <- par_info$out_dir
    if (is.null(ref_year)) ref_year <- par_info$ref_year
  }
  file <- file.path(out_dir, "WaterH_out.dat")
  if (!file.exists(file)) {
    cli::cli_abort(c("x" = "No {.file WaterH_out.dat} in {.path {out_dir}}."))
  }
  d <- read_simstrat_dat_file(file)
  data.frame(Date = as.Date(simstrat_day_to_date(d$day, ref_year)),
             LKE_lvlwtr = as.vector(d$values[, 1]))
}
