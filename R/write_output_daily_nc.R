#' Write a daily-mean companion to a model's sub-daily netCDF output
#'
#' GOTM-WET can emit a native daily-mean output stream (its `output_daily`
#' block, `time_method: mean`). GLM-AED and Simstrat have no equivalent, so
#' when `time(aeme)$output_daily_mean` is `TRUE` [run_aeme()] calls this
#' function after the run to build the same thing by post-processing: it reads
#' the model's raw sub-daily `output.nc`, averages every time-indexed variable
#' over each calendar day, and writes `output_daily.nc` next to it. The raw
#' sub-daily file is left in place (both are kept).
#'
#' The daily file mirrors the source file's structure --- same dimensions
#' (with `time` replaced by one value per day, stamped at 00:00 UTC), same
#' `units`/`long_name` attributes, same compression. A variable that is not
#' indexed by `time` (a coordinate such as the layer or zone axis) is copied
#' through unchanged. Averaging uses `na.rm = TRUE`; a day with no data for a
#' cell becomes `NA`.
#'
#' Only the variables the model's reader consumes are carried over --- the
#' targeted `vars` mapped through [key_naming], plus, for GLM-AED, the fixed
#' set [read_glm_output()] reads unconditionally (`z`, `NS`, the daily surface
#' fluxes and volume/area/level diagnostics, ...). GLM's raw `output.nc`
#' cannot itself be sub-selected, so this is where a GLM run's stored output
#' is actually pruned; skipping its large 4-D fields (`light`, `umean`, the
#' wave spectra, ...) is most of the size and time saving.
#'
#' @param lake_dir character; the lake directory (the parent of the model's
#'   own sub-directory).
#' @param model character; one of `"glm_aed"`, `"simstrat_aed2"`,
#'   `"simstrat_aed"`.
#' @param vars character or `NULL`; AEME variable names (e.g. `"HYD_temp"`) to
#'   keep in the daily file, mapped to the model's own output names via
#'   [key_naming]. The internals AEME's readers always need are added
#'   automatically. `NULL` (default) keeps every variable in the source file.
#'
#' @return Invisibly, the path to the written `output_daily.nc`, or `NULL` if
#'   the source `output.nc` could not be found.
#'
#' GLM-AED output variables [read_glm_output()] / [read_glm_wlev()] read
#' unconditionally, regardless of the targeted variable set. Keep these in the
#' daily companion so the reader still works against it. (`z` is GLM's
#' time-varying layer-height dim-variable, emitted separately; listing it here
#' is harmless.)
#' @noRd
.glm_daily_keep_extra <- c(
  "time", "z", "NS", "lake_level",
  "daily_qe", "daily_qh", "daily_qlw", "daily_qsw",
  "lake_volume", "evaporation", "evap_mass_flux", "surface_area",
  "tot_inflow_vol", "overflow_vol", "tot_outflow_vol",
  "precipitation", "surface_temp",
  "radn"
)

#' @importFrom ncdf4 nc_open nc_close ncvar_get ncatt_get ncdim_def ncvar_def
#' @importFrom ncdf4 nc_create ncvar_put
#' @keywords internal
#' @noRd
write_output_daily_nc <- function(lake_dir, model, vars = NULL) {

  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")
  model <- check_model(model)

  src <- tryCatch(
    get_model_outfile(model = model, path = lake_dir)[[model]],
    error = function(e) character(0))
  if (!is.null(names(src)) && "output" %in% names(src)) src <- src[["output"]]
  src <- src[nzchar(src) & file.exists(src)]
  if (length(src) == 0) {
    cli::cli_warn(c("!" = "No {.val {model}} {.file output.nc} to build a
                    daily-mean file from."))
    return(invisible(NULL))
  }
  src <- src[[1]]
  dst <- .output_daily_path(src)

  nc <- ncdf4::nc_open(src)
  on.exit(ncdf4::nc_close(nc), add = TRUE)

  if (!"time" %in% names(nc$dim)) {
    cli::cli_warn(c("!" = "{.val {model}} {.file output.nc} has no {.field time}
                    dimension; skipping daily-mean file."))
    return(invisible(NULL))
  }

  # attribute helper: a character value only when the attribute really exists
  .att_chr <- function(varid, name, default = "") {
    a <- ncdf4::ncatt_get(nc, varid, name)
    if (isTRUE(a$hasatt) && is.character(a$value) && nzchar(a$value)) {
      a$value
    } else {
      default
    }
  }

  # --- source time axis -> calendar-day groups
  tvals <- as.numeric(ncdf4::ncvar_get(nc, "time"))
  tunit <- .att_chr("time", "units", "seconds since 1970-01-01 00:00:00")
  tcal <- .att_chr("time", "calendar", NA_character_)

  # The time DIMENSION is not always literally named "time" (GLM names it
  # differently from the coordinate variable). Identify it as the unlimited
  # dimension, falling back to the one whose length matches the time axis.
  tdim_name <- {
    unlim <- vapply(nc$dim, function(d) isTRUE(d$unlim), logical(1))
    if (any(unlim)) {
      names(nc$dim)[which(unlim)[1]]
    } else {
      hit <- vapply(nc$dim, function(d) isTRUE(d$len == length(tvals)),
                    logical(1))
      if (any(hit)) names(nc$dim)[which(hit)[1]] else "time"
    }
  }
  to_posix <- function(v) {
    if (grepl("^hours since", tunit)) {
      as.POSIXct(gsub("hours since ", "", tunit), tz = "UTC") + v * 3600
    } else if (grepl("^seconds since", tunit)) {
      as.POSIXct(gsub("seconds since ", "", tunit), tz = "UTC") + v
    } else if (grepl("^days since", tunit)) {
      as.POSIXct(gsub("days since ", "", tunit), tz = "UTC") + v * 86400
    } else {
      as.POSIXct(v, origin = "1970-01-01", tz = "UTC")
    }
  }
  posix <- to_posix(tvals)
  # Close-of-day convention: a timestamp at 00:00 belongs to the day that just
  # ended (matches GLM's daily diagnostics and GOTM's period-end mean stamp).
  day <- as.Date(posix - 1, tz = "UTC")
  ud <- sort(unique(day))
  grp <- match(day, ud)
  cols_by_day <- split(seq_along(day), grp)
  ndays <- length(ud)

  # New daily time values, in the source file's own units, stamped at 00:00.
  day_posix <- as.POSIXct(paste0(ud, " 00:00:00"), tz = "UTC")
  new_time <- if (grepl("^hours since", tunit)) {
    as.numeric(difftime(day_posix,
                        as.POSIXct(gsub("hours since ", "", tunit), tz = "UTC"),
                        units = "hours"))
  } else if (grepl("^seconds since", tunit)) {
    as.numeric(difftime(day_posix,
                        as.POSIXct(gsub("seconds since ", "", tunit), tz = "UTC"),
                        units = "secs"))
  } else if (grepl("^days since", tunit)) {
    as.numeric(difftime(day_posix,
                        as.POSIXct(gsub("days since ", "", tunit), tz = "UTC"),
                        units = "days"))
  } else {
    as.numeric(day_posix)
  }

  # Average an array over its time axis into per-day means.
  agg_time <- function(x, tax) {
    dm <- dim(x)
    if (is.null(dm)) dm <- length(x)
    if (length(dm) == 1L) {
      r <- vapply(cols_by_day, function(ix) mean(x[ix], na.rm = TRUE),
                  numeric(1), USE.NAMES = FALSE)
    } else {
      perm <- c(setdiff(seq_along(dm), tax), tax)
      xp <- aperm(x, perm)
      dmp <- dim(xp)
      other <- prod(dmp[-length(dmp)])
      xm <- matrix(xp, nrow = other)
      aggm <- vapply(cols_by_day, function(ix)
        rowMeans(xm[, ix, drop = FALSE], na.rm = TRUE), numeric(other))
      aggp <- array(aggm, dim = c(dmp[-length(dmp)], ndays))
      r <- aperm(aggp, order(perm))
    }
    r[is.nan(r)] <- NA_real_
    r
  }

  # Some models (GLM-AED's `z`) store a coordinate as a *time-varying* 2-D
  # dimension-variable: `nc$dim[[nm]]$vals` is a matrix, not a length-`len`
  # vector, and ncdf4 does not list it in `nc$var`. Rebuild such a dimension
  # as a plain index axis and re-emit the coordinate as a real 2-D variable,
  # time-averaged, so the model's reader still finds it.
  tv_dim <- vapply(names(nc$dim), function(nm) {
    !identical(nm, tdim_name) &&
      length(nc$dim[[nm]]$vals) > nc$dim[[nm]]$len
  }, logical(1))
  tv_dim_names <- names(nc$dim)[tv_dim]

  var_dim_names <- function(v) vapply(nc$var[[v]]$dim, `[[`, character(1),
                                      "name")
  var_is_time <- function(v) tdim_name %in% var_dim_names(v)

  # --- which variables to keep: the targeted `vars` (mapped to model names)
  # plus each model's reader-required internals. GLM's own `output.nc` cannot
  # be sub-selected (see set_output_vars()), so this companion is where a GLM
  # run's stored output is pruned.
  all_vars <- names(nc$var)
  drop_particle <- vapply(all_vars, function(v)
    "particle" %in% var_dim_names(v), logical(1))
  keep <- all_vars[!drop_particle]
  if (!is.null(vars) && length(vars)) {
    # Some AEME variables (derived ones) have no model output name; that is
    # expected here, so do not surface .map_output_vars()'s warning.
    native <- tryCatch(suppressWarnings(.map_output_vars(vars, model)),
                       error = function(e) character(0))
    if ("glm_aed" %in% model) native <- union(native, .glm_daily_keep_extra)
    not_time <- !vapply(keep, var_is_time, logical(1))
    sel <- unique(c(keep[not_time], intersect(native, keep)))
    if (length(sel)) keep <- sel
  }

  # --- rebuild dimensions (time swapped for the daily axis; time-varying
  # dimvars collapsed to a plain index axis)
  has_cal <- !is.na(tcal)
  used_dims <- unique(c(unlist(lapply(keep, var_dim_names)), tv_dim_names,
                        tdim_name))
  new_dims <- lapply(stats::setNames(used_dims, used_dims), function(nm) {
    d <- nc$dim[[nm]]
    if (identical(nm, tdim_name)) {
      if (has_cal) {
        ncdf4::ncdim_def(nm, units = tunit, vals = new_time, unlim = TRUE,
                         calendar = tcal)
      } else {
        ncdf4::ncdim_def(nm, units = tunit, vals = new_time, unlim = TRUE)
      }
    } else if (nm %in% tv_dim_names) {
      ncdf4::ncdim_def(nm, units = "", vals = seq_len(d$len),
                       create_dimvar = FALSE)
    } else {
      vals <- if (!is.null(d$vals) && length(d$vals) == d$len) d$vals
              else seq_len(d$len)
      ncdf4::ncdim_def(nm, units = if (is.character(d$units)) d$units else "",
                       vals = vals, unlim = isTRUE(d$unlim))
    }
  })

  prec_ok <- c("float", "double", "integer", "short", "char", "byte")
  mk_var <- function(name, dnames, units = "", longname = name,
                     missval = NA, prec = "double", compression = 5) {
    ncdf4::ncvar_def(name = name, units = units,
                     dim = unname(new_dims[dnames]),
                     missval = missval, longname = longname,
                     prec = if (prec == "char") "double" else prec,
                     compression = compression)
  }
  new_vars <- lapply(keep, function(v) {
    vd <- nc$var[[v]]
    prec <- if (!is.null(vd$prec) && vd$prec %in% prec_ok) vd$prec else "double"
    mk_var(v, var_dim_names(v), units = .att_chr(v, "units", ""),
           longname = .att_chr(v, "long_name", v),
           missval = if (is.numeric(vd$missval) &&
                         length(vd$missval) == 1L) vd$missval else NA,
           prec = prec,
           compression = if (is.numeric(vd$compression) &&
                             length(vd$compression) == 1L &&
                             !is.na(vd$compression) &&
                             vd$compression > 0) vd$compression else 5)
  })
  names(new_vars) <- keep
  for (nm in tv_dim_names) {
    new_vars[[nm]] <- mk_var(nm, c(nm, tdim_name))
  }

  out <- ncdf4::nc_create(dst, new_vars)
  on.exit(ncdf4::nc_close(out), add = TRUE)

  for (v in keep) {
    x <- ncdf4::ncvar_get(nc, v, collapse_degen = FALSE)
    if (!var_is_time(v)) {
      ncdf4::ncvar_put(out, v, x)
    } else {
      tax <- which(var_dim_names(v) == tdim_name)
      ncdf4::ncvar_put(out, v, agg_time(x, tax))
    }
  }
  for (nm in tv_dim_names) {
    m <- nc$dim[[nm]]$vals
    if (is.null(dim(m))) dim(m) <- c(nc$dim[[nm]]$len, length(tvals))
    ncdf4::ncvar_put(out, nm, agg_time(m, 2L))
  }

  invisible(dst)
}
