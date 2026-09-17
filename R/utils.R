#' Check and convert time input to POSIXct
#'
#' @param x character, Date, or POSIXt object representing time
#' @param tz character; timezone, default is "UTC"
#' 
#' @importFrom cli cli_abort
#'
#' @returns POSIXct object
#' @noRd
check_time_format <- function(x, tz = "UTC") {
  if (inherits(x, "Date")) return(as.POSIXct(x, tz = tz))
  if (inherits(x, "POSIXt")) return(as.POSIXct(x, tz = tz))
  
  if (is.character(x)) {
    formats <- c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d")
    for (fmt in formats) {
      parsed <- as.POSIXct(x, format = fmt, tz = tz)
      if (!any(is.na(parsed))) return(parsed)
    }
    cli::cli_abort(
      c(
        "!" = "Invalid time format detected.",
        "x" = "Input must be in one of: {.val 'YYYY-mm-dd HH:MM:SS'}, {.val 'YYYY-mm-dd HH:MM'}, {.val 'YYYY-mm-dd'}.",
        "i" = "Alternatively, provide a {.cls Date} or {.cls POSIXt} object."
      ),
      class = "aeme_error_time_format"
    )
  }

  cli::cli_abort(
    c(
      "!" = "{.arg x} must be a {.cls character}, {.cls Date}, or {.cls POSIXt} object.",
      "x" = "You supplied a {.cls {class(x)[1]}}."
    ),
    class = "aeme_error_time_type"
  )
}


#' Interpret user-supplied timestamps in a declared timezone, store as UTC
#'
#' The ingest-boundary conversion. Everything downstream of this in AEME assumes
#' UTC. Rules:
#' \itemize{
#'   \item `Date` (daily) input is returned unchanged -- a timezone is
#'     meaningless for a calendar date and must never shift it.
#'   \item character / naive `POSIXct` input (no `tzone`, or an empty one) is
#'     taken to be wall-clock time in `tz` and converted to the equivalent UTC
#'     instant.
#'   \item a `POSIXct` carrying an explicit, non-UTC `tzone` is an absolute
#'     instant: it is re-expressed in UTC, never re-localised.
#'   \item a `POSIXct` tagged `"UTC"` is ambiguous -- `"UTC"` is also R's default
#'     fallback label (`read_csv()`, `read_yaml()`, `as.POSIXct()` on many
#'     paths). With `reinterpret_utc_tag = TRUE` and a non-UTC `tz`, such a
#'     column is taken to be wall-clock time in `tz` (the user declared a data
#'     timezone, so their file's `"UTC"` tag is treated as unreliable). With the
#'     default `FALSE` it is left as an absolute instant.
#' }
#'
#' @param x Date, POSIXct, or character vector of timestamps.
#' @param tz character; Olson timezone the naive input is expressed in.
#' @param reinterpret_utc_tag logical; treat a `"UTC"`-tagged `POSIXct` as naive
#'   wall-clock time in `tz` when `tz` is not `"UTC"`. Use at the single ingest
#'   call for a data stream; keep `FALSE` everywhere the data may already have
#'   been through ingest, so re-running never double-shifts.
#' @return `Date` unchanged, or a UTC `POSIXct`.
#' @noRd
.to_utc <- function(x, tz = "UTC", reinterpret_utc_tag = FALSE) {
  if (is.null(x) || !length(x)) return(x)
  if (inherits(x, "Date")) return(x)

  if (is.character(x) || is.factor(x)) {
    # check_time_format() anchors the wall-clock string in `tz`; with_tz then
    # re-expresses that same instant in UTC.
    return(lubridate::with_tz(check_time_format(as.character(x), tz = tz), "UTC"))
  }

  if (inherits(x, "POSIXt")) {
    tzone <- attr(x, "tzone")
    is_naive <- is.null(tzone) || !nzchar(tzone)
    treat_as_naive <- is_naive ||
      (identical(tzone, "UTC") && isTRUE(reinterpret_utc_tag) &&
         !identical(tz, "UTC"))
    if (treat_as_naive) {
      # The clock reading is wall time in `tz` (tz may be "UTC" -- a no-op).
      # roll_dst keeps the spring-forward gap hour from becoming NA: gap times
      # roll to the boundary, and an ambiguous fall-back hour takes the later
      # offset.
      x <- lubridate::force_tz(as.POSIXct(x), tzone = tz,
                               roll_dst = c("boundary", "post"))
    }
    return(lubridate::with_tz(x, "UTC"))
  }

  cli::cli_abort(
    c("!" = "{.arg x} must be a {.cls character}, {.cls Date}, or {.cls POSIXt} object.",
      "x" = "You supplied a {.cls {class(x)[1]}}."),
    class = "aeme_error_time_type"
  )
}

#' Parse a forcing date column, preserving sub-daily resolution
#'
#' Reads a `Date`/`POSIXct`/character column of forcing timestamps. Sub-daily
#' data is returned as `POSIXct` (UTC); daily data is returned as `Date` so the
#' rest of the daily pipeline is unaffected. AEME never disaggregates -- this
#' only preserves whatever resolution the user supplied.
#'
#' @param x Date, POSIXct, or character vector.
#' @param tz character; timezone a naive **sub-daily** series is expressed in.
#'   Daily input is treated as calendar dates and is never shifted, whatever
#'   `tz` is.
#' @param reinterpret_utc_tag logical; passed to `.to_utc()`. Only set `TRUE` at
#'   the single ingest call for a stream.
#' @return Date (daily input) or UTC POSIXct (sub-daily input).
#' @noRd
.as_forcing_datetime <- function(x, tz = "UTC", reinterpret_utc_tag = FALSE) {
  # Decide resolution on the unshifted parse so a daily calendar series is
  # never moved across a day boundary by the timezone conversion.
  xt <- check_time_format(x, tz = "UTC")
  if (is_subdaily(xt)) {
    .to_utc(x, tz = tz, reinterpret_utc_tag = reinterpret_utc_tag)
  } else {
    as.Date(xt)
  }
}


#' Parse an observation date column to noon-anchored POSIXct
#'
#' Observations are daily in meaning but are stored as `POSIXct` (UTC) so they
#' can be matched against a sub-daily model axis without the midnight
#' calendar-day-boundary ambiguity. Daily input -- a `Date`, a date-only
#' string, or a `POSIXct` whose stamps all sit at UTC midnight -- is anchored
#' at `<date> 12:00:00` UTC. Input that already carries a genuine time-of-day
#' (sub-daily observations, or a column already anchored at noon) is converted
#' to UTC and returned unchanged; this also makes the function idempotent.
#'
#' Unlike `.as_forcing_datetime()` the resolution test is "does any stamp carry
#' a time-of-day", not median spacing -- an observation column legitimately has
#' many rows on one calendar date (a depth profile), which a spacing heuristic
#' would misread as sub-daily.
#'
#' @param x Date, POSIXct, or character vector.
#' @param tz character; timezone a naive **time-carrying** input is expressed
#'   in. A calendar date is never shifted, whatever `tz` is.
#' @param reinterpret_utc_tag logical; passed to `.to_utc()` for time-carrying
#'   input. Only set `TRUE` at the single ingest call for a data stream.
#' @return UTC `POSIXct`.
#' @noRd
.as_obs_datetime <- function(x, tz = "UTC", reinterpret_utc_tag = FALSE) {
  if (is.null(x) || !length(x)) return(x)

  # Resolve the resolution on an UNSHIFTED parse first -- as .as_forcing_datetime
  # does -- so a daily calendar series is never moved across a day boundary by
  # the timezone conversion below. check_time_format() rejects a character
  # vector holding NA, so probe only the finite entries.
  ok <- !is.na(x) & (!is.character(x) | nzchar(trimws(as.character(x))))
  probe <- rep(as.POSIXct(NA, tz = "UTC"), length(x))
  if (any(ok)) probe[ok] <- check_time_format(x[ok], tz = "UTC")
  secs  <- as.numeric(probe) %% 86400
  has_tod <- !(is.na(probe) | secs < 1 | secs > 86399)

  if (!inherits(x, "Date") && any(has_tod)) {
    # Time-carrying input (sub-daily obs, or a column already anchored at noon).
    # Take the true absolute instant in UTC and leave it -- this also makes the
    # function idempotent on an already-converted column.
    out <- rep(as.POSIXct(NA, tz = "UTC"), length(x))
    if (any(ok)) out[ok] <- as.POSIXct(.to_utc(x[ok], tz = tz,
                              reinterpret_utc_tag = reinterpret_utc_tag),
                            tz = "UTC")
    return(out)
  }

  # Daily: anchor every row at noon UTC of its (unshifted) calendar day.
  # Built from the numeric day count so NA rows propagate cleanly.
  .POSIXct(as.numeric(as.Date(probe, tz = "UTC")) * 86400 + 43200, tz = "UTC")
}


#' Is a date/time vector sub-daily?
#'
#' Returns \code{TRUE} when the median spacing between successive timestamps is
#' shorter than one day. A \code{Date} vector, or a \code{POSIXct} vector whose
#' rows all fall at midnight, is treated as daily (\code{FALSE}). Used to decide
#' whether the daily code paths (which collapse to \code{as.Date()}) can be kept.
#'
#' @param x Date, POSIXct, or character vector of timestamps.
#' @return logical scalar.
#' @noRd
is_subdaily <- function(x) {
  if (is.null(x) || length(x) < 2) return(FALSE)
  if (inherits(x, "Date")) return(FALSE)
  xt <- tryCatch(as.POSIXct(x, tz = "UTC"), error = function(e) NULL)
  if (is.null(xt) || all(is.na(xt))) return(FALSE)
  xt <- sort(xt[!is.na(xt)])
  if (length(xt) < 2) return(FALSE)
  steps <- as.numeric(diff(xt), units = "secs")
  med <- stats::median(steps, na.rm = TRUE)
  isTRUE(is.finite(med) && med < 86400)
}


#' Abort if object is not a data frame
#'
#' Utility function to check that an object is a data frame or tibble.
#'
#' @param df Object to check.
#' @param name Optional name of the object (for informative messages).
#' @param class_suffix Optional string appended to the error class.
#'
#' @returns Invisibly returns the object if it is a data frame; otherwise aborts.
#' @noRd
abort_if_not_dataframe <- function(df, name = NULL, class_suffix = NULL) {
  if (!is.data.frame(df)) {
    cli::cli_abort(
      c(
        "!" = "{.arg {name %||% 'object'}} must be a data frame, not {.cls {class(df)[1]}}."
      ),
      class = c("aeme_error_type", paste0("aeme_error_", class_suffix %||% name))
    )
  }
  
  invisible(df)
}

#' Abort if required columns are missing
#'
#' Utility function to check for required columns in a data frame or tibble.
#'
#' @param df A data frame or tibble.
#' @param required_cols Character vector of required column names.
#' @param name Optional name of the data frame for informative messages (e.g. "met", "hypsograph").
#' @param class_suffix Optional string appended to the error class (default: name of data frame if provided).
#'
#' @returns Invisibly returns the data frame if valid; otherwise aborts with a `cli_abort` error.
#' @noRd
abort_if_missing_cols <- function(df, required_cols, name = NULL, class_suffix = NULL) {
  stopifnot(is.character(required_cols))
  
  missing_cols <- setdiff(required_cols, colnames(df))
  
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      c(
        "!" = "Missing required columns in {.arg {name %||% 'data frame'}}.",
        "x" = "Missing: {paste(missing_cols, collapse = ', ')}",
        "i" = "Expected: {paste(required_cols, collapse = ', ')}"
      ),
      class = c("aeme_error_missing_cols", paste0("aeme_error_", class_suffix %||% name))
    )
  }
  
  invisible(df)
}

#' Check hypsograph data frame
#'
#' @param hypsograph data frame with columns "depth", "area" and "elev". Depth should be
#' monotonic decreasing and area should be monotonic increasing.
#' If NULL, the function will check for hypsograph in the input slot of the Aeme object.
#' @inheritParams build_aeme
#' @returns Invisibly returns the hypsograph data frame if aeme is NULL, otherwise returns the Aeme object.
#' @importFrom cli cli_abort
#' @noRd
#' @importFrom dplyr arrange desc

check_hypsograph <- function(hypsograph, aeme = NULL) {
  if (!is.null(aeme)) {
    inp <- input(aeme)
    hypsograph <- inp$hypsograph
    if (is.null(hypsograph)) {
      cli::cli_abort(
        c(
          "!" = "No hypsograph found in {.arg aeme$input}.",
          "i" = "Please add a hypsograph data frame to the {.arg input} slot before proceeding."
        ),
        class = "aeme_error_hypsograph_missing"
      )
    }
  }
  
  # Validate data frame
  abort_if_not_dataframe(hypsograph, name = "hypsograph")
  abort_if_missing_cols(hypsograph, c("depth", "area", "elev"), name = "hypsograph")
  
  # Ensure depth descending and area ascending
  hypsograph <- dplyr::arrange(hypsograph, dplyr::desc(depth))
  
  if (any(diff(hypsograph$depth) >= 0)) {
    cli::cli_abort("{.arg depth} must be strictly monotonic decreasing.",
                   class = "aeme_error_hypsograph_depth")
  }
  if (any(diff(hypsograph$area) >= 0)) {
    cli::cli_abort("{.arg area} must be strictly monotonic increasing.",
                   class = "aeme_error_hypsograph_area")
  }
  
  invisible(if (is.null(aeme)) hypsograph else aeme)
}

#' Check if object is a valid Aeme object
#' @param aeme object to check
#' @returns Invisibly returns the Aeme object if valid, otherwise throws an
#' error.
#' @importFrom cli cli_abort
#' @importFrom methods slotNames
#' @export
check_aeme <- function(aeme) {
  if (!inherits(aeme, "Aeme")) {
    cli::cli_abort(
      "{.arg aeme} must be an {.cls Aeme} object, not {.cls {class(aeme)[1]}}.",
      class = "aeme_error_aeme_type"
    )
  }

  required_slots <- c("lake", "time", "input", "inflows", "outflows",
                      "water_balance", "parameters")
  missing_slots <- setdiff(required_slots, methods::slotNames(aeme))

  if (length(missing_slots) > 0) {
    cli::cli_abort(
      c(
        "!" = "The {.cls Aeme} object is missing required slots.",
        "x" = "Missing: {paste(missing_slots, collapse = ', ')}",
        "i" = "Expected slots: {paste(required_slots, collapse = ', ')}"
      ),
      class = "aeme_error_aeme_slots"
    )
  }

  aeme <- migrate_aeme(aeme)

  built_version <- aeme@configuration$aeme_version
  installed_version <- utils::packageVersion("AEME")
  if (is.null(built_version)) {
    cli::cli_warn(
      c("!" = "This {.cls Aeme} object has no recorded AEME package version.",
        "i" = "It was likely built with an older version of AEME (<0.4.0), or has
        never been built with {.fn build_aeme}. Consider rebuilding with
        {.fn build_aeme} to keep it in sync with the installed package
        ({installed_version})."),
      class = "aeme_warning_version_missing",
      .frequency = "once",
      .frequency_id = "aeme_warning_version_missing"
    )
  } else if (package_version(built_version) < installed_version) {
    cli::cli_warn(
      c("!" = "This {.cls Aeme} object was built with AEME {built_version},
        but the installed version is {installed_version}.",
        "i" = "Consider rebuilding with {.fn build_aeme} if you encounter
        unexpected behaviour."),
      class = "aeme_warning_version_outdated",
      .frequency = "once",
      .frequency_id = paste0("aeme_warning_version_outdated_", built_version)
    )
  }

  invisible(aeme)
}

#' Apply structural migrations to an older Aeme object (silent, idempotent)
#'
#' Older `Aeme` objects (loaded from `.rds`/`.yaml` files, or already held in
#' memory from an earlier package version) predate models such as
#' `simstrat_aed2`/`simstrat_aed` and use since-renamed list elements. Most
#' read paths in the package use `[[`, which tolerates a missing list element
#' (returns `NULL`), but a few (e.g. the `show()` method's
#' `round(inf$factor$simstrat_aed2, 2)`, or `build_aeme()`'s outflow-elevation
#' handling) pass that `NULL` straight into a function that errors on it.
#'
#' This is the low-level worker: it is silent, idempotent, and cheap enough to
#' run on every `show()`/`plot()`. [upgrade_aeme()] is the user-facing wrapper
#' that also reports what changed and fills in build-time scalar defaults.
#'
#' Migrations applied here:
#' \itemize{
#'  \item `time$spin_up`, `inflows$factor`, `outflows$factor`,
#'    `configuration`: backfill entries for any model in `list_models()` not
#'    already present, using the same defaults `aeme_constructor()` would.
#'  \item `outflows`: rename the legacy `lvl` (<= 0.2.x) / `outflow_lvl`
#'    (0.3.x) element to `elevation`.
#'  \item `output`: add a `NULL` placeholder per model and coerce
#'    `n_members` to integer.
#'  \item `observations$level`: coerce a legacy tibble to a plain data frame
#'    and ensure a `var_aeme` column.
#'  \item `observations$lake`: collapse the legacy `depth_from` / `depth_to`
#'    column pair to a single `depth` column (interval midpoint).
#'  \item `observations$lake` / `observations$level`: convert a `Date` `Date`
#'    column to noon-anchored UTC `POSIXct` (`<date> 12:00:00`).
#' }
#'
#' @param aeme An Aeme object.
#' @return The Aeme object, migrated to the current layout.
#' @keywords internal
#' @noRd
migrate_aeme <- function(aeme) {
  if (!inherits(aeme, "Aeme")) return(aeme)

  models <- unname(list_models())

  # -- time$spin_up: backfill entries for models added after this object ------
  aeme_time <- aeme@time
  if (is.list(aeme_time$spin_up)) {
    for (m in setdiff(models, names(aeme_time$spin_up))) aeme_time$spin_up[[m]] <- 2
    aeme@time <- aeme_time
  }

  # -- time$tz: legacy objects predate the declared input timezone. Their
  # start/stop were stored as UTC (or were broken); "UTC" is the safe,
  # reproducible assumption and a no-op for already-absolute instants.
  if (is.null(aeme@time$tz)) {
    aeme@time$tz <- "UTC"
  }

  # -- inflows$factor: backfill entries for new models -----------------------
  inf <- aeme@inflows
  if (is.list(inf$factor)) {
    for (m in setdiff(models, names(inf$factor))) inf$factor[[m]] <- 1
    aeme@inflows <- inf
  }

  # -- outflows: backfill factors + rename lvl/outflow_lvl -> elevation ------
  outf <- aeme@outflows
  if (is.list(outf$factor)) {
    for (m in setdiff(models, names(outf$factor))) outf$factor[[m]] <- 1
  }
  if (is.null(outf$elevation)) {
    legacy_lvl <- outf$lvl %||% outf$outflow_lvl
    if (!is.null(legacy_lvl) && length(outf$data) > 0) {
      outf$elevation <- stats::setNames(
        as.list(rep(legacy_lvl[1], length(outf$data))), names(outf$data)
      )
    } else {
      outf$elevation <- -1
    }
  }
  outf$lvl <- NULL
  outf$outflow_lvl <- NULL
  aeme@outflows <- outf

  # -- configuration: backfill per-model hydrodynamic/bgc sublists ----------
  cfg <- aeme@configuration
  if (is.list(cfg)) {
    for (m in setdiff(models, names(cfg))) {
      cfg[[m]] <- list(hydrodynamic = NULL, bgc = NULL)
    }
    aeme@configuration <- cfg
  }

  # -- output: NULL placeholder per model + integer n_members --------------
  outp <- aeme@output
  if (is.list(outp)) {
    if (is.null(outp$n_members)) outp$n_members <- 0L
    outp$n_members <- as.integer(outp$n_members)
    for (m in models) if (!m %in% names(outp)) outp[m] <- list(NULL)
    aeme@output <- outp
  }

  # -- observations$level: legacy tibble -> data.frame + var_aeme ----------
  obs <- aeme@observations
  if (!is.null(obs$level)) {
    if (!is.data.frame(obs$level) || inherits(obs$level, "tbl_df")) {
      obs$level <- as.data.frame(obs$level, stringsAsFactors = FALSE)
    }
    if (!"var_aeme" %in% names(obs$level)) obs$level[["var_aeme"]] <- "LKE_lvlwtr"
    aeme@observations <- obs
  }

  # -- observations$lake: legacy depth_from / depth_to -> depth -----------
  if (!is.null(obs$lake) && "depth_from" %in% names(obs$lake) &&
      !"depth" %in% names(obs$lake)) {
    obs$lake <- normalise_lake_obs(obs$lake)
    aeme@observations <- obs
  }

  # -- observations$lake / $level Date: Date -> noon-anchored UTC POSIXct --
  # Guarded on `Date`-class so this is a no-op once converted (keeps the
  # migration idempotent). Sub-daily observation columns already carrying a
  # time-of-day are left as they are by .as_obs_datetime().
  if (!is.null(obs$lake) && "Date" %in% names(obs$lake) &&
      inherits(obs$lake$Date, "Date")) {
    obs$lake$Date <- .as_obs_datetime(obs$lake$Date)
    aeme@observations <- obs
  }
  if (!is.null(obs$level) && "Date" %in% names(obs$level) &&
      inherits(obs$level$Date, "Date")) {
    obs$level$Date <- .as_obs_datetime(obs$level$Date)
    aeme@observations <- obs
  }

  aeme
}

#' Upgrade an Aeme object to the current AEME version
#'
#' @description
#' Older `Aeme` objects -- loaded from `.rds` files written by a previous
#' version of AEME -- can be missing list elements, use since-renamed slot
#' names, or carry data frames with an older column layout. Most of the
#' package tolerates this, but some code paths (and [build_aeme()] in
#' particular) assume the current layout.
#'
#' `upgrade_aeme()` applies every structural migration AEME knows about, in
#' order, each one idempotent so the function is safe to run repeatedly. It
#' does **not** rebuild model configuration
#' (`configuration$<model>$hydrodynamic`) or model output -- those only come
#' from [build_aeme()] / [run_aeme()]. Run `upgrade_aeme()` first, then
#' rebuild if you need the model files refreshed.
#'
#' Migrations applied (see also migrate_aeme(), the silent worker):
#' \itemize{
#'  \item `time$spin_up`, `inflows$factor`, `outflows$factor`,
#'    `configuration`: backfill entries for models added to AEME after the
#'    object was created (e.g. `simstrat_aed2`, `simstrat_aed`).
#'  \item `outflows`: rename the legacy `lvl` / `outflow_lvl` element to
#'    `elevation`.
#'  \item `output`: add a `NULL` placeholder per model and coerce
#'    `n_members` to integer.
#'  \item `observations$level`: coerce a legacy tibble to a plain data frame
#'    and ensure a `var_aeme` column.
#'  \item `observations$lake`: collapse the legacy `depth_from` / `depth_to`
#'    column pair to a single `depth` column (interval midpoint), keeping
#'    `depth_to` only where it records a genuine integrated sample.
#'  \item `observations$lake` / `observations$level`: convert a `Date` `Date`
#'    column to noon-anchored UTC `POSIXct` (`<date> 12:00:00`), so daily
#'    observations match a sub-daily model axis unambiguously.
#'  \item `configuration`: backfill scalar build defaults (`ext_elev`,
#'    `calc_wbal`, `wb_method`, `calc_wlev`, `hum_type`, `est_swr_hr`,
#'    `use_bgc`) from `config_defaults()`.
#'  \item `parameters`: reorder columns to [param_colnames()] order.
#' }
#'
#' @param aeme An `Aeme` object.
#' @param quiet Logical; suppress the summary of applied changes. Default
#'   `FALSE`.
#' @return The `Aeme` object, migrated to the current layout, with
#'   `configuration$aeme_upgraded` set to the installed AEME version.
#' @seealso [build_aeme()], [check_aeme()]
#' @importFrom cli cli_abort cli_inform
#' @importFrom utils packageVersion
#' @export
upgrade_aeme <- function(aeme, quiet = FALSE) {
  if (!inherits(aeme, "Aeme")) {
    cli::cli_abort(
      "{.arg aeme} must be an {.cls Aeme} object, not {.cls {class(aeme)[1]}}.",
      class = "aeme_error_aeme_type"
    )
  }

  before <- aeme
  aeme <- migrate_aeme(aeme)
  changed <- character()

  bn <- function(x) if (is.null(x)) character() else names(x)
  note_added <- function(label, old, new) {
    d <- setdiff(bn(new), bn(old))
    if (length(d))
      changed <<- c(changed, sprintf("%s: added %s", label,
                                     paste(d, collapse = ", ")))
  }
  note_added("time$spin_up", before@time$spin_up, aeme@time$spin_up)
  note_added("inflows$factor", before@inflows$factor, aeme@inflows$factor)
  note_added("outflows$factor", before@outflows$factor, aeme@outflows$factor)
  note_added("configuration", before@configuration, aeme@configuration)
  if (is.null(before@outflows$elevation) && !is.null(aeme@outflows$elevation))
    changed <- c(changed,
                 "outflows: `lvl`/`outflow_lvl` renamed to `elevation`")
  if (!identical(bn(before@output), bn(aeme@output)))
    changed <- c(changed, "output: added per-model placeholders")
  if (inherits(before@observations$level, "tbl_df") &&
      !inherits(aeme@observations$level, "tbl_df"))
    changed <- c(changed, "observations$level: coerced tibble to data.frame")
  if (!is.null(before@observations$lake) &&
      "depth_from" %in% names(before@observations$lake) &&
      "depth" %in% names(aeme@observations$lake) &&
      !"depth_from" %in% names(aeme@observations$lake))
    changed <- c(changed,
                 "observations$lake: `depth_from`/`depth_to` collapsed to `depth`")
  obs_date_migrated <- vapply(c("lake", "level"), function(slot) {
    inherits(before@observations[[slot]]$Date, "Date") &&
      inherits(aeme@observations[[slot]]$Date, "POSIXct")
  }, logical(1))
  if (any(obs_date_migrated))
    changed <- c(changed,
                 sprintf("observations$%s$Date: `Date` anchored to noon UTC `POSIXct`",
                         paste(names(which(obs_date_migrated)), collapse = "/")))

  # -- scalar configuration defaults (cold path only) ---------------------
  cfg <- aeme@configuration
  cfg_dflt <- config_defaults()
  scalar_keys <- c("ext_elev", "calc_wbal", "wb_method", "calc_wlev",
                   "hum_type", "est_swr_hr", "use_bgc")
  added_cfg <- character()
  for (k in scalar_keys) {
    if (is.null(cfg[[k]])) {
      cfg[[k]] <- cfg_dflt[[k]]
      added_cfg <- c(added_cfg, k)
    }
  }
  if (length(added_cfg))
    changed <- c(changed, sprintf("configuration: filled defaults for %s",
                                  paste(added_cfg, collapse = ", ")))
  aeme@configuration <- cfg

  # -- parameters column order ------------------------------------------
  params <- aeme@parameters
  want <- param_colnames(incl_opt = FALSE)
  if (all(want %in% names(params)) &&
      !identical(names(params)[seq_along(want)], want)) {
    aeme@parameters <- params[, c(want, setdiff(names(params), want)),
                              drop = FALSE]
    changed <- c(changed, "parameters: columns reordered to canonical layout")
  }

  target <- as.character(utils::packageVersion("AEME"))
  aeme@configuration$aeme_upgraded <- target

  if (!quiet) {
    from <- before@configuration$aeme_version
    from_lbl <- if (is.null(from)) "an unversioned (pre-0.4.0) build" else
      paste0("v", from)
    if (length(changed) == 0) {
      cli::cli_inform(c(
        "v" = "{.cls Aeme} object already matches AEME {target}; nothing to upgrade."
      ))
    } else {
      cli::cli_inform(c(
        "v" = "Upgraded {.cls Aeme} object from {from_lbl} to AEME {target}:",
        stats::setNames(changed, rep("*", length(changed))),
        "i" = "Model configuration and output are not migrated - rerun {.fn build_aeme} to refresh them."
      ))
    }
  }

  aeme
}


#' Check meteorological data frame
#' @param met data frame 
#' @returns Invisibly returns the met data frame if valid, otherwise throws an 
#' error.
#' @importFrom cli cli_abort
#' @noRd
check_met <- function(met) {
  # Validate data frame and required columns
  abort_if_not_dataframe(met, name = "met")
  abort_if_missing_cols(met, c("Date", "MET_radswd", "MET_tmpair", "MET_pprain"), name = "met")

  # Check wind columns
  wind1 <- "MET_wndspd"
  wind2 <- c("MET_wnduvu", "MET_wnduvv")
  if (!wind1 %in% colnames(met) && !all(wind2 %in% colnames(met))) {
    cli::cli_abort(
      c(
        "!" = "{.arg met} must contain either:",
        "*" = "{.val MET_wndspd}",
        "or" = "both {.val MET_wnduvu} and {.val MET_wnduvv}."
      ),
      class = "aeme_error_met_wind"
    )
  }

  # Check for missing values in required columns
  if (any(is.na(met[, c("Date", "MET_radswd", "MET_tmpair", "MET_pprain")]))) {
    cli::cli_abort("Missing values detected in required meteorological columns.",
                   class = "aeme_error_met_na")
  }

  # Check Date column type. POSIXct is accepted so sub-daily meteo can be
  # supplied at its native resolution (AEME does not disaggregate).
  if (!inherits(met$Date, c("Date", "POSIXct"))) {
    cli::cli_abort(paste0("{.arg met$Date} must be a {.cls Date} or {.cls ",
                          "POSIXct} object, not {.cls {class(met$Date)[1]}}."),
                   class = "aeme_error_met_date")
  }

  invisible(met)
}


#' Format ensemble member label
#' @param ens_n integer; ensemble member number
#' @returns character; formatted ensemble member label
#' @noRd
format_ens_label <- function(ens_n) {
  paste0("ens_", sprintf("%03d", ens_n))
}

#' Return mean sea level pressure given air temperature, elevation and station pressure.
#'
#' @param prsttn A numeric vector of observed station pressure in Pa
#' @param elevation A numeric vector of elevation in m
#' @param tmpair A numeric vector of air temperature in degC
#'
#' @return A numeric vector of mean sea level pressure in Pa
#'
#' @references
#' Hess SL, Introduction to theoretical meteorology, Holt Rinehart and Winston, NY 1959,
#' ch. 6.5; Stull RB, Meteorology for scientists and engineers, 2nd edition,
#' Brooks/Cole 2000, ch. 1.
#'
#' @note
#' The standard procedure for the US is to use for MET_tmpair the average
#' of the current station temperature and the station temperature from 12 hours ago.
#'
#' @examples
#' get_mean_sea_level_pressure(101226.5, 105:205, 17.19)
#'
#' @export
get_mean_sea_level_pressure <- function(prsttn, elevation, tmpair) {
  # Calculate average temperature in column of air, assuming a lapse rate
  # of 6.5 degC/km
  t_column <- tmpair + 0.0065 * elevation / 2
  # Determine the scale height
  h <- 287.055 * (t_column + 273.15) / 9.807
  # Calculate the mean sea level pressure
  prsttn * exp(elevation / h)
}

#' Return station pressure from mean sea level pressure.
#'
#' @param prmslp A numeric vector of mean sea level pressure in Pa
#' @param elevation A numeric vector of elevation in m
#' @param tmpair A numeric vector of air temperature in degC
#'
#' @return A numeric vector of station pressure in Pa
#'
#' @references See \code{\link{get_mean_sea_level_pressure}}.
#'
#' @note
#' This function is just the inverse of \code{\link{get_mean_sea_level_pressure}}.
#'
#' @examples
#' get_station_pressure(101226.5, 105:205, 17.19)
#'
#' @export
get_station_pressure <- function(prmslp, elevation, tmpair) {
  prmslp / get_mean_sea_level_pressure(1, elevation, tmpair)
}
