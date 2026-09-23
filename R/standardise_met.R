#' Standardise meteorological variable names and units for AEME
#'
#' Attempts to match column names in a meteorological data frame to AEME
#' standard variable names using \code{guess_aeme_vars()}, then detects the
#' likely input units of each variable from its values and converts to the
#' units expected by the package.
#'
#' @section AEME standard variables and units:
#' \tabular{llll}{
#'   \strong{Variable}   \tab \strong{Name}       \tab \strong{Unit} \tab \strong{Required} \cr
#'   Shortwave radiation \tab \code{MET_radswd}    \tab W/m²          \tab Yes \cr
#'   Air temperature     \tab \code{MET_tmpair}    \tab °C            \tab Yes \cr
#'   Wind speed          \tab \code{MET_wndspd}    \tab m/s           \tab Yes \cr
#'   Rainfall            \tab \code{MET_pprain}    \tab mm/day         \tab Yes \cr
#'   Snowfall            \tab \code{MET_ppsnow}    \tab mm/day         \tab No (defaults to 0) \cr
#'   u wind component    \tab \code{MET_wnduvu}    \tab m/s           \tab No (derivable) \cr
#'   v wind component    \tab \code{MET_wnduvv}    \tab m/s           \tab No (derivable) \cr
#'   Sea-level pressure  \tab \code{MET_prmslp}    \tab Pa            \tab No (derivable) \cr
#'   Station pressure    \tab \code{MET_prsttn}    \tab Pa            \tab No (derivable) \cr
#'   Cloud cover         \tab \code{MET_cldcvr}    \tab 1 (fraction)  \tab No (derivable) \cr
#'   Longwave radiation  \tab \code{MET_radlwd}    \tab W/m²          \tab No (derivable) \cr
#'   Dew point temp.     \tab \code{MET_tmpdew}    \tab °C            \tab No (derivable) \cr
#'   Vapour pressure     \tab \code{MET_prvapr}    \tab hPa           \tab No (derivable) \cr
#'   Relative humidity   \tab \code{MET_humrel}    \tab \%            \tab No (derivable) \cr
#'   Wind direction      \tab \code{MET_wnddir}    \tab degrees       \tab No (derivable) \cr
#' }
#'
#' @param met data.frame; meteorological data with a \code{Date} column and
#'   one or more meteorological variable columns.
#' @param verbose logical; if \code{TRUE} (default), emit \code{cli_inform}
#'   messages describing each detected unit conversion applied. Set to
#'   \code{FALSE} for quiet operation inside pipelines.
#' @param precip_accum logical; how to interpret \code{MET_pprain} /
#'   \code{MET_ppsnow} when the data is sub-daily. \code{TRUE} (default) treats
#'   them as the depth accumulated \emph{within each step} (the ERA5 / AWS
#'   convention) and rescales to the mm/day rate the rest of AEME expects;
#'   \code{FALSE} takes the values to be a mm/day rate already and leaves them
#'   untouched. Ignored for daily data, where the two are identical.
#' @param tz character; Olson timezone in which a naive/character \code{Date}
#'   column is expressed. Sub-daily timestamps are converted to UTC; daily data
#'   is treated as calendar dates and never shifted. A column that already
#'   carries a timezone (including \code{"UTC"}) is taken at face value here --
#'   reinterpreting a \code{"UTC"}-tagged column against a declared local zone
#'   happens once, upstream, in \code{\link{add_met}} /
#'   \code{\link{aeme_constructor}}. Default \code{"UTC"}; \code{\link{build_aeme}}
#'   passes the object's \code{time$tz}.
#' @param longitude numeric; lake longitude in decimal degrees (east positive).
#'   When supplied and the data is sub-daily, the hour at which
#'   \code{MET_radswd} peaks each day is compared against astronomical solar
#'   noon for that longitude; a warning is emitted if they differ by more than
#'   3 h, which usually means the timestamps are in local time rather than UTC.
#'   \code{NULL} (default) skips the check. \code{\link{build_aeme}} passes the
#'   lake longitude.
#'
#' @return The input data frame with column names remapped to AEME standard
#'   names and values converted to AEME standard units where a conversion
#'   was necessary. Columns that could not be matched are retained unchanged
#'   with a warning. A warning is also emitted if any required variable
#'   (\code{MET_radswd}, \code{MET_tmpair}, \code{MET_wndspd},
#'   \code{MET_pprain}) is absent after renaming.
#'
#' @importFrom cli cli_abort cli_warn cli_inform
#' @export
standardise_met <- function(met, verbose = TRUE, precip_accum = TRUE,
                            tz = "UTC", longitude = NULL) {

  # Internal datetime arithmetic runs in UTC; `tz` is applied explicitly to the
  # incoming Date column only.
  withr::local_locale(c("LC_TIME" = "C"))
  withr::local_timezone("UTC")

  # ── Input validation ──────────────────────────────────────────────────────

  if (!is.data.frame(met)) {
    cli::cli_abort(
      c("{.arg met} must be a {.cls data.frame}.",
        "x" = "Got {.cls {class(met)}}."),
      class = "aeme_error_met_type"
    )
  }
  if (nrow(met) == 0) {
    cli::cli_abort(
      c("{.arg met} has no rows.",
        "i" = "Supply a non-empty meteorological data frame."),
      class = "aeme_error_met_empty"
    )
  }
  met <- .rename_date_column(met, verbose = verbose, arg_name = "met", tz = tz)
  if (!"Date" %in% names(met)) {
    cli::cli_abort(
      c("{.arg met} must contain a {.code Date} column.",
        "x" = "Columns found: {.val {names(met)}}."),
      class = "aeme_error_met_no_date"
    )
  }
  
  # ── Step 1: remap column names ───────────────────────────────────────────
  
  met <- .rename_met_columns(met, verbose = verbose)
  
  # ── Step 2: warn if required variables are missing ───────────────────────

  required <- c("MET_radswd", "MET_tmpair", "MET_wndspd", "MET_pprain")
  missing_required <- setdiff(required, names(met))
  if (length(missing_required) > 0) {
    cli::cli_warn(
      c("!" = "Required variable{?s} {.val {missing_required}}
          {?is/are} absent after renaming.",
        "i" = "AEME models cannot run without these variables."),
      class = "aeme_warn_met_missing_required"
    )
  }

  # ── Step 2b: sanity-check sub-daily shortwave against solar noon ──────────
  .check_solar_noon_offset(met, longitude = longitude)


  # ── Step 3: detect units and convert ────────────────────────────────────

  met <- .convert_met_units(met, verbose = verbose)

  # ── Step 4: sub-daily precip accumulation -> mm/day rate ────────────────

  if (isTRUE(precip_accum)) {
    met <- .subdaily_precip_to_rate(met, verbose = verbose)
  }

  met
}


#' Rescale sub-daily precipitation accumulations to a mm/day rate
#'
#' AEME defines `MET_pprain` / `MET_ppsnow` as a rate in mm/day, and every model
#' met-writer (`make_met_glm()` `/1000`, `make_met_simstrat()` `/1000/24`,
#' `make_met_gotm()` `/86400`) assumes that. Sub-daily reanalysis products
#' instead report the depth that fell *within each step* (mm per hour, say).
#' The two are numerically identical for daily data; for sub-daily data the
#' per-step accumulation is scaled up by `86400 / step_seconds`. Applied
#' automatically to sub-daily meteo (see `standardise_met(precip_accum=)`).
#' @noRd
.subdaily_precip_to_rate <- function(met, verbose = TRUE) {
  vars <- intersect(c("MET_pprain", "MET_ppsnow"), names(met))
  if (!length(vars) || !"Date" %in% names(met)) return(met)
  if (!is_subdaily(met[["Date"]])) return(met)

  ordered_t <- sort(as.POSIXct(met[["Date"]], tz = "UTC"))
  step <- stats::median(as.numeric(diff(ordered_t), units = "secs"),
                        na.rm = TRUE)
  if (!is.finite(step) || step <= 0 || step >= 86400) return(met)

  scale <- 86400 / step
  for (v in vars) met[[v]] <- met[[v]] * scale

  if (verbose) {
    cli::cli_inform(c(
      "i" = paste0("Sub-daily meteo: rescaled {.val {vars}} from a per-",
                   "{round(step)}-s accumulation to a mm/day rate ",
                   "(x{signif(scale, 4)})."),
      "i" = paste("Pass {.code standardise_met(precip_accum = FALSE)} if these",
                  "are already a mm/day rate.")
    ), class = "aeme_inform_met_precip_rate")
  }
  met
}


# ── Internal: column renaming ─────────────────────────────────────────────────

#' @noRd
.report_timestep <- function(dates) {

  if (length(dates) < 2) {
    cli::cli_inform(
      c("i" = "Only one timestamp present; cannot determine timestep."),
      class = "aeme_inform_met_timestep"
    )
    return(invisible(NULL))
  }

  # Differences in seconds
  diffs_secs <- as.numeric(diff(as.POSIXct(dates, tz = "UTC")), units = "secs")
  median_secs <- stats::median(diffs_secs, na.rm = TRUE)
  
  timestep_label <- dplyr::case_when(
    median_secs < 60                  ~ paste(round(median_secs), "second(s)"),
    median_secs < 3600                ~ paste(round(median_secs / 60), "minute(s)"),
    median_secs < 86400               ~ paste(round(median_secs / 3600), "hour(s)"),
    median_secs < 86400 * 7           ~ paste(round(median_secs / 86400), "day(s)"),
    median_secs < 86400 * 31         ~ paste(round(median_secs / (86400 * 7)), "week(s)"),
    TRUE                              ~ paste(round(median_secs / (86400 * 30.44)), "month(s)")
  )
  
  irregular <- (stats::sd(diffs_secs, na.rm = TRUE) / median_secs) > 0.05
  
  if (irregular) {
    cli::cli_inform(
      c("i" = "Detected irregular timestep (median: {timestep_label}).",
        "!" = "Gaps or irregular intervals were found in the date/time column."),
      class = "aeme_inform_met_timestep"
    )
  } else {
    # cli::cli_inform(
    #   c("i" = "Detected regular timestep: {timestep_label}."),
    #   class = "aeme_inform_met_timestep"
    # )
  }
}

#' Warn when sub-daily shortwave peaks far from astronomical solar noon
#'
#' A cheap heuristic for the common "meteo timestamps are in local time, not
#' UTC" mistake. For sub-daily data with a `MET_radswd` column and a known
#' longitude, it finds the circular-mean hour-of-day at which shortwave peaks
#' and compares it to solar noon (`12 - longitude / 15`, in UTC hours). A
#' difference greater than 3 h is almost always a timezone offset. Never
#' aborts; skips silently when it cannot decide.
#'
#' @param met data.frame; after column renaming (needs `Date` + `MET_radswd`).
#' @param longitude numeric(1); east-positive decimal degrees, or NULL to skip.
#' @noRd
.check_solar_noon_offset <- function(met, longitude = NULL) {
  if (is.null(longitude) || length(longitude) != 1L || !is.finite(longitude)) {
    return(invisible(NULL))
  }
  d <- met[["Date"]]
  if (is.null(d) || !inherits(d, "POSIXct")) return(invisible(NULL))  # daily
  if (!"MET_radswd" %in% names(met)) return(invisible(NULL))

  sw <- suppressWarnings(as.numeric(met[["MET_radswd"]]))
  keep <- is.finite(sw) & !is.na(d)
  if (sum(keep) < 24) return(invisible(NULL))
  d <- d[keep]; sw <- sw[keep]

  day <- as.Date(d, tz = "UTC")
  # fractional hour-of-day (on the Date column's own clock) of each day's peak
  peak_hr <- tapply(seq_along(sw), day, function(ix) {
    if (all(sw[ix] <= 0)) return(NA_real_)
    j <- ix[which.max(sw[ix])]
    as.numeric(difftime(d[j], as.POSIXct(as.character(day[j]), tz = "UTC"),
                        units = "hours"))
  })
  peak_hr <- peak_hr[is.finite(peak_hr)]
  if (length(peak_hr) < 2) return(invisible(NULL))

  # circular mean, so peaks either side of midnight (true-UTC data at eastern
  # longitudes) average correctly
  ang <- peak_hr / 24 * 2 * pi
  obs <- (atan2(mean(sin(ang)), mean(cos(ang))) / (2 * pi) * 24) %% 24
  expected <- (12 - longitude / 15) %% 24
  diff_h <- ((obs - expected + 12) %% 24) - 12  # signed, in (-12, 12]
  if (abs(diff_h) <= 3) return(invisible(NULL))

  hm <- function(x) {
    x <- x %% 24
    h <- floor(x); m <- round((x - h) * 60)
    if (m == 60L) { m <- 0L; h <- (h + 1) %% 24 }
    sprintf("%02d:%02d", as.integer(h), as.integer(m))
  }
  cli::cli_warn(
    c("!" = paste0("Sub-daily {.code MET_radswd} peaks around {hm(obs)} on the ",
                   "meteo clock, ~{abs(round(diff_h))} h from solar noon ",
                   "(~{hm(expected)} UTC) for longitude {round(longitude, 2)}."),
      "i" = "This usually means the timestamps are in local time, not UTC.",
      "i" = paste("Supply UTC timestamps, or declare the source zone via",
                  "{.code build_aeme(tz = ...)} / {.code set_time(tz = ...)}",
                  "so AEME converts them.")),
    class = "aeme_warn_met_solar_offset"
  )
  invisible(NULL)
}

#' @noRd
.rename_date_column <- function(data, verbose, arg_name = "data", tz = "UTC") {

  # If "Date" column already exists, localise it and report the timestep.
  # `.as_forcing_datetime()` keeps the Date/POSIXct hybrid: a daily calendar
  # series is returned as `Date` (never shifted); a sub-daily series is taken to
  # be wall-clock time in `tz` and returned as UTC `POSIXct`. Already-UTC input
  # is a no-op.
  if ("Date" %in% names(data)) {
    data[["Date"]] <- .as_forcing_datetime(data[["Date"]], tz = tz)
    if (verbose) .report_timestep(data$Date)
    return(data)
  }

  # Try to find a date/time column among the column names
  keywords <- c("date", "time", "datetime", "timestamp", "dt")
  col_match <- names(data)[tolower(names(data)) %in% keywords]
  
  # Broader keyword search if no exact match
  if (length(col_match) == 0) {
    col_match <- names(data)[grepl("date|time", names(data), ignore.case = TRUE)]
  }
  
  if (length(col_match) == 0) {
    cli::cli_abort(
      c("!" = "No date/time column found in {.arg {arg_name}}.",
        "i" = "Expected a column named {.val Date} or containing {.val date} or {.val time}."),
      class = "aeme_error_no_date"
    )
  }
  
  if (length(col_match) > 1) {
    cli::cli_warn(
      c("!" = "Multiple potential date/time columns found in {.arg {arg_name}}: {.val {col_match}}.",
        "i" = "Using the first match: {.val {col_match[1]}}."),
      class = "aeme_warn_multiple_date"
    )
  }
  
  date_col <- col_match[1]

  # Parse + localise: naive/character timestamps are wall-clock time in `tz`;
  # sub-daily values become UTC POSIXct, daily values stay `Date` (unshifted).
  data[[date_col]] <- tryCatch(
    .as_forcing_datetime(data[[date_col]], tz = tz),
    error = function(e) {
      cli::cli_abort(
        c("!" = "Could not parse column {.val {date_col}} in {.arg {arg_name}} as a date/time.",
          "x" = conditionMessage(e)),
        class = "aeme_error_date_parse"
      )
    }
  )

  if (verbose) {
    cli::cli_inform(
      c("i" = "Renaming date/time column in {.arg {arg_name}}: {.val {date_col}} \u2192 {.val Date}"),
      class = "aeme_inform_date_rename"
    )
  }

  names(data)[names(data) == date_col] <- "Date"

  if (verbose) .report_timestep(data$Date)

  data
}

#' @noRd
.rename_met_columns <- function(met, verbose) {
  
  non_date <- setdiff(names(met), "Date")
  if (length(non_date) == 0) {
    cli::cli_warn(
      c("!" = "{.arg met} contains only a {.code Date} column.",
        "i" = "No meteorological variables to rename or convert."),
      class = "aeme_warn_met_no_vars"
    )
    return(met)
  }
  pot_met_vars <- lookup_aeme_vars(group = "MET") |> 
    dplyr::pull(var_aeme)
  
  if (all(non_date %in% pot_met_vars)) {
    # All columns already match AEME standard names; skip guessing
    # if (verbose) {
    #   cli_inform_safe(c("i" = "All columns already match AEME standard variable names,
    #     skipping name guessing."))
    # }
    return(met)
  }
  
  # guess_aeme_vars() returns a named character vector mapping original names
  # to AEME standard names (NA where no match was found).
  guessed <- tryCatch(
    guess_aeme_vars(non_date, key_filter = "MET"),
    error = function(e) {
      cli::cli_abort(
        c("Failed to call {.fn guess_aeme_vars}.",
          "x" = conditionMessage(e)),
        class = "aeme_error_guess_vars",
        call = NULL
      )
    }
  )
  names(guessed) <- non_date
  
  matched   <- guessed[!is.na(guessed)]
  unmatched <- non_date[is.na(guessed)]
  
  if (length(unmatched) > 0) {
    cli::cli_warn(
      c("!" = "{length(unmatched)} column{?s} could not be matched to a known
          AEME variable and will be left unchanged.",
        "x" = "Unmatched: {.val {unmatched}}."),
      class = "aeme_warn_met_unmatched"
    )
  }
  
  if (length(matched) > 0 && verbose) {
    rename_bullets <- setNames(
      paste0(names(matched), " \u2192 ", matched),
      rep("*", length(matched))
    )
    cli::cli_inform(
      c("i" = "Renaming {length(matched)} column{?s} to AEME standard names:",
        rename_bullets),
      class = "aeme_inform_met_rename"
    )
  }
  
  # Apply the renames
  idx <- match(names(matched), names(met))
  names(met)[idx] <- matched
  
  met
}


# ── Internal: unit detection and conversion ───────────────────────────────────

# Each entry in the conversion table defines four things:
#   detect  — function(x) returning TRUE when the column values look like
#             they are in the wrong units. Uses median/max on non-NA, non-zero
#             values to be robust to sparse or all-zero columns (e.g. snow).
#   convert — function(x) returning the converted values.
#   from    — human-readable label for the detected input unit.
#   to      — human-readable label for the AEME target unit.
#
# Detection relies on domain knowledge about plausible value ranges:
#
#   MET_tmpair / tmpdew / airmax / airmin / dewmax / dewmin
#     °C:  typical range −40 to +50.  Median > 100 → almost certainly Kelvin.
#     K:   typical range 233 to 323.
#
#   MET_radswd / radlwd
#     W/m²:      daily mean typically 50–400.
#     MJ/m²/day: daily mean typically 4–35.  Median < 50 → likely MJ/m²/day.
#     kJ/m²/day: daily mean typically 4000–35000. Median > 5000 → likely kJ.
#
#   MET_humrel
#     %:        0–100.  max > 1 confirms percent already.
#     fraction: 0–1.   max ≤ 1 → multiply by 100.
#
#   MET_cldcvr
#     fraction: 0–1.   max ≤ 1 → already correct.
#     oktas:    0–8.   max > 1 → divide by 8.
#
#   MET_prsttn / prmslp
#     Pa:  typical 95000–106000.  Median > 10000 → already Pa.
#     hPa: typical 950–1060.      Median < 2000  → multiply by 100.
#
#   MET_prvapr
#     hPa: typical 5–50.     Median < 200  → already hPa.
#     Pa:  typical 500–5000. Median > 200  → divide by 100.
#
#   MET_wndspd / wnduvu / wnduvv
#     m/s:  typical 0–30.   Median < 30 → already m/s.
#     km/h: typical 0–108.  Median > 30 → divide by 3.6.
#     knots: typical 0–60.  Median 30–60 → ambiguous; flag a warning.
#
#   MET_pprain / ppsnow
#     m/day:  typical 0–0.1.   max ≤ 0.5  → already m/day.
#     mm/day: typical 0–100.   max > 0.5  → divide by 1000.

.met_conversion_table <- list(
  
  # ── Temperature variables (°C expected; detect Kelvin) ──────────────────
  MET_tmpair = list(
    detect  = function(x) .nz_median(x) > 100,
    convert = function(x) x - 273.15,
    from    = "Kelvin (K)",
    to      = "Celsius (\u00b0C)"
  ),
  MET_tmpdew = list(
    detect  = function(x) .nz_median(x) > 100,
    convert = function(x) x - 273.15,
    from    = "Kelvin (K)",
    to      = "Celsius (\u00b0C)"
  ),
  MET_airmax = list(
    detect  = function(x) .nz_median(x) > 100,
    convert = function(x) x - 273.15,
    from    = "Kelvin (K)",
    to      = "Celsius (\u00b0C)"
  ),
  MET_airmin = list(
    detect  = function(x) .nz_median(x) > 100,
    convert = function(x) x - 273.15,
    from    = "Kelvin (K)",
    to      = "Celsius (\u00b0C)"
  ),
  MET_dewmax = list(
    detect  = function(x) .nz_median(x) > 100,
    convert = function(x) x - 273.15,
    from    = "Kelvin (K)",
    to      = "Celsius (\u00b0C)"
  ),
  MET_dewmin = list(
    detect  = function(x) .nz_median(x) > 100,
    convert = function(x) x - 273.15,
    from    = "Kelvin (K)",
    to      = "Celsius (\u00b0C)"
  ),
  
  # ── Radiation (W/m² expected; detect MJ/m²/day or kJ/m²/day) ───────────
  MET_radswd = list(
    detect  = function(x) {
      med <- .nz_median(x)
      if (med > 5000) return("kJ")   # kJ/m²/day
      if (med < 50)   return("MJ")   # MJ/m²/day
      FALSE
    },
    convert = function(x, flag) {
      if (flag == "kJ") return(x / 86.4)   # kJ/m²/day -> W/m²
      if (flag == "MJ") return(x / 0.0864) # MJ/m²/day -> W/m²
      x
    },
    from_fn = function(flag) if (flag == "kJ") "kJ/m\u00b2/day" else "MJ/m\u00b2/day",
    to      = "W/m\u00b2"
  ),
  MET_radlwd = list(
    detect  = function(x) {
      med <- .nz_median(x)
      if (med > 5000) return("kJ")
      if (med < 50)   return("MJ")
      FALSE
    },
    convert = function(x, flag) {
      if (flag == "kJ") return(x / 86.4)
      if (flag == "MJ") return(x / 0.0864)
      x
    },
    from_fn = function(flag) if (flag == "kJ") "kJ/m\u00b2/day" else "MJ/m\u00b2/day",
    to      = "W/m\u00b2"
  ),
  
  # ── Relative humidity (% expected; detect 0–1 fraction) ─────────────────
  MET_humrel = list(
    detect  = function(x) max(x, na.rm = TRUE) <= 1,
    convert = function(x) x * 100,
    from    = "fraction (0\u20131)",
    to      = "percent (0\u2013100 %)"
  ),
  
  # ── Cloud cover (fraction 0–1 expected; detect oktas 0–8) ───────────────
  MET_cldcvr = list(
    detect  = function(x) max(x, na.rm = TRUE) > 1,
    convert = function(x) x / 8,
    from    = "oktas (0\u20138)",
    to      = "fraction (0\u20131)"
  ),
  
  # ── Pressure (Pa expected; detect hPa/mbar) ─────────────────────────────
  MET_prsttn = list(
    detect  = function(x) .nz_median(x) < 2000,
    convert = function(x) x * 100,
    from    = "hPa / mbar",
    to      = "Pa"
  ),
  MET_prmslp = list(
    detect  = function(x) .nz_median(x) < 2000,
    convert = function(x) x * 100,
    from    = "hPa / mbar",
    to      = "Pa"
  ),
  
  # ── Vapour pressure (hPa expected; detect Pa) ────────────────────────────
  MET_prvapr = list(
    detect  = function(x) .nz_median(x) > 200,
    convert = function(x) x / 100,
    from    = "Pa",
    to      = "hPa"
  ),
  
  # ── Wind speed (m/s expected; detect km/h) ──────────────────────────────
  # Knots (median 30–60) are ambiguous with km/h; a separate warning is
  # emitted when the median falls in the knots range after no conversion
  # was triggered, so the user can inspect.
  MET_wndspd = list(
    detect  = function(x) .nz_median(x) > 30,
    convert = function(x) x / 3.6,
    from    = "km/h",
    to      = "m/s"
  ),
  MET_wnduvu = list(
    detect  = function(x) abs(.nz_median(x)) > 30,
    convert = function(x) x / 3.6,
    from    = "km/h",
    to      = "m/s"
  ),
  MET_wnduvv = list(
    detect  = function(x) abs(.nz_median(x)) > 30,
    convert = function(x) x / 3.6,
    from    = "km/h",
    to      = "m/s"
  ),
  
  # ── Precipitation (mm/day expected; detect m/day) ───────────────────────
  # Snow is often all-zero; only attempt detection when non-zero values exist.
  MET_pprain = list(
    # m/day would have max << 0.5; mm/day is the expected standard now
    detect  = function(x) .has_nonzero(x) && max(x, na.rm = TRUE) < 0.5,
    convert = function(x) x * 1000,
    from    = "m/day",
    to      = "mm/day"
  ),
  MET_ppsnow = list(
    detect  = function(x) .has_nonzero(x) && max(x, na.rm = TRUE) < 0.5,
    convert = function(x) x * 1000,
    from    = "mm/day",
    to      = "m/day"
  )
)


#' @noRd
.convert_met_units <- function(met, verbose) {
  
  for (var in names(.met_conversion_table)) {
    
    if (!var %in% names(met)) next
    
    spec <- .met_conversion_table[[var]]
    x    <- met[[var]]
    
    # Skip entirely if column is all NA
    if (all(is.na(x))) {
      if (verbose) {
        cli::cli_inform(
          c("i" = "{.code {var}}: all NA, skipping unit detection."),
          class = "aeme_inform_met_all_na"
        )
      }
      next
    }
    
    # Radiation variables have a two-stage detect that returns a string flag
    # rather than a plain logical, to distinguish MJ from kJ.
    flag <- spec$detect(x)
    
    if (isFALSE(flag) || is.null(flag)) {
      # No conversion needed — emit an informational note when verbose
      # if (verbose) {
      #   msg <- paste0(var, ": values appear to be in the expected units, no conversion applied.")
      #   cli_inform_safe(c("i" = msg))
      # }
      next
    }
    
    # Determine human-readable from-unit label
    if (!is.null(spec$from_fn)) {
      from_label <- spec$from_fn(flag)
    } else {
      from_label <- spec$from
    }
    
    # Apply conversion (radiation passes the flag; others ignore extra args)
    if (!is.null(spec$from_fn)) {
      met[[var]] <- spec$convert(x, flag)
    } else {
      met[[var]] <- spec$convert(x)
    }
    
    if (verbose) {
      cli::cli_inform(
        c("v" = "{.code {var}}: converted from {from_label} to {spec$to}."),
        class = "aeme_inform_met_converted"
      )
    }
  }
  
  # ── Post-conversion sanity checks ────────────────────────────────────────
  
  met <- .sanity_check_met(met)
  
  met
}


# ── Internal: post-conversion sanity checks ────────────────────────────────────

#' @noRd
.sanity_check_met <- function(met) {
  
  checks <- list(
    list(
      var  = "MET_tmpair",
      test = function(x) any(x < -90 | x > 60, na.rm = TRUE),
      msg  = "Values outside plausible air temperature range [-90, 60] \u00b0C.",
      class = "aeme_warn_met_sanity_tmpair"
    ),
    list(
      var  = "MET_tmpdew",
      test = function(x) any(x < -90 | x > 60, na.rm = TRUE),
      msg  = "Values outside plausible dew point range [-90, 60] \u00b0C.",
      class = "aeme_warn_met_sanity_tmpdew"
    ),
    list(
      var  = "MET_humrel",
      test = function(x) any(x < 0 | x > 100, na.rm = TRUE),
      msg  = "Values outside valid relative humidity range [0, 100] %.",
      class = "aeme_warn_met_sanity_humrel"
    ),
    list(
      var  = "MET_cldcvr",
      test = function(x) any(x < 0 | x > 1, na.rm = TRUE),
      msg  = "Values outside valid cloud cover fraction range [0, 1].",
      class = "aeme_warn_met_sanity_cldcvr"
    ),
    list(
      var  = "MET_radswd",
      test = function(x) any(x < 0, na.rm = TRUE),
      msg  = "Negative shortwave radiation values detected.",
      class = "aeme_warn_met_sanity_radswd"
    ),
    list(
      var  = "MET_radlwd",
      test = function(x) any(x < 0, na.rm = TRUE),
      msg  = "Negative longwave radiation values detected.",
      class = "aeme_warn_met_sanity_radlwd"
    ),
    list(
      var  = "MET_wndspd",
      test = function(x) any(x < 0, na.rm = TRUE),
      msg  = "Negative wind speed values detected.",
      class = "aeme_warn_met_sanity_wndspd"
    ),
    list(
      var  = "MET_wnddir",
      test = function(x) any(x < 0 | x > 360, na.rm = TRUE),
      msg  = "Wind direction values outside [0, 360] degrees.",
      class = "aeme_warn_met_sanity_wnddir"
    ),
    list(
      var  = "MET_pprain",
      test = function(x) any(x < 0 | x > 500, na.rm = TRUE),  # 500 mm/day is extreme but physically possible
      msg  = "Rainfall values outside plausible range [0, 500] mm/day.",
      class = "aeme_warn_met_sanity_pprain"
    ),
    list(
      var  = "MET_ppsnow",
      test = function(x) any(x < 0, na.rm = TRUE),
      msg  = "Negative snowfall values detected.",
      class = "aeme_warn_met_sanity_ppsnow"
    ),
    list(
      var  = "MET_prsttn",
      test = function(x) any(x < 80000 | x > 110000, na.rm = TRUE),
      msg  = "Station pressure values outside plausible range [80000, 110000] Pa.",
      class = "aeme_warn_met_sanity_prsttn"
    ),
    list(
      var  = "MET_prmslp",
      test = function(x) any(x < 87000 | x > 108500, na.rm = TRUE),
      msg  = "Sea-level pressure values outside plausible range [87000, 108500] Pa.",
      class = "aeme_warn_met_sanity_prmslp"
    )
  )
  
  for (chk in checks) {
    if (!chk$var %in% names(met)) next
    x <- met[[chk$var]]
    if (all(is.na(x))) next
    if (chk$test(x)) {
      cli::cli_warn(
        c("!" = "{.code {chk$var}}: {chk$msg}",
          "i" = "Check raw data or unit conversion for this variable."),
        class = chk$class
      )
    }
  }
  
  met
}


# ── Utility helpers ────────────────────────────────────────────────────────────

#' Median of non-NA, non-zero values; returns NA if none exist
#' @noRd
.nz_median <- function(x) {
  vals <- x[!is.na(x) & x != 0]
  if (length(vals) == 0) return(NA_real_)
  median(vals)
}

#' TRUE if there are any non-NA, non-zero values
#' @noRd
.has_nonzero <- function(x) {
  any(!is.na(x) & x != 0)
}
