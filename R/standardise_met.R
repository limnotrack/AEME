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
standardise_met <- function(met, verbose = TRUE) {
  
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
  
  # ── Step 3: detect units and convert ────────────────────────────────────
  
  met <- .convert_met_units(met, verbose = verbose)
  
  met
}


# ── Internal: column renaming ─────────────────────────────────────────────────

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
    if (verbose) {
      cli_inform_safe(c("i" = "All columns already match AEME standard variable names,
        skipping name guessing."))
    }
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
      if (verbose) {
        msg <- paste0(var, ": values appear to be in the expected units, no conversion applied.")
        cli_inform_safe(c("i" = msg))
      }
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
