# Observations recorded relative to the lake bed.
#
# Lake observations are normally depths below the water surface. Some are
# recorded as a height above the bed (a logger 1 m above the sediment). The
# stored `depth` column is always metres below the surface, so everything that
# reads it stays correct; a bottom-referenced observation also carries
#
#   * `depth_ref`        "surface" or "bottom": how it was recorded;
#   * `height_above_bed` metres above the lake bed, for "bottom" rows.
#
# `depth` of a bottom row was worked out from a water level when it was added,
# which is an estimate. Code that compares observations with model output
# (`get_var(use_obs = TRUE)`, `ensemble_summary()`) uses `height_above_bed` with
# the model's own water level instead, which is what "1 m above the bed" means.

obs_depth_ref_values <- c("surface", "bottom")

#' Standardise the optional `depth_ref` / `height_above_bed` observation columns
#'
#' Does nothing to observations without a `depth_ref` column. Otherwise missing
#' values become `"surface"` and values are checked. With `require_height`,
#' every bottom-referenced row must also have a non-negative `height_above_bed`:
#' that is how they are stored. Input to [add_obs()] has the height in `depth`
#' and gets its `height_above_bed` there, so it is not checked on the way in.
#' @param lake data.frame of lake observations, or `NULL`.
#' @param require_height logical; check `height_above_bed` of bottom rows.
#' @return `lake`, with `depth_ref` filled in and `height_above_bed` numeric.
#' @noRd
normalise_obs_depth_ref <- function(lake, require_height = FALSE) {
  if (!is.data.frame(lake) || !"depth_ref" %in% names(lake)) return(lake)

  ref <- as.character(lake$depth_ref)
  ref[is.na(ref)] <- "surface"
  bad <- setdiff(unique(ref), obs_depth_ref_values)
  if (length(bad)) {
    cli::cli_abort(
      c("{.field depth_ref} must be {.val surface} or {.val bottom}.",
        "x" = "Got {.val {bad}}."),
      class = "aeme_error_obs_depth_ref")
  }
  lake$depth_ref <- ref

  if ("height_above_bed" %in% names(lake)) {
    lake$height_above_bed <- as.numeric(lake$height_above_bed)
  }
  is_bottom <- ref == "bottom"
  if (require_height && any(is_bottom)) {
    h <- if ("height_above_bed" %in% names(lake)) lake$height_above_bed else
      rep(NA_real_, nrow(lake))
    if (anyNA(h[is_bottom]) || any(h[is_bottom] < 0)) {
      cli::cli_abort(
        c("Bottom-referenced observations need a non-negative
           {.field height_above_bed}.",
          "i" = "Use {.fn add_obs} to add them: it takes the height in
                 {.field depth} and works out the depth below the surface."),
        class = "aeme_error_obs_depth_ref")
    }
  }
  lake
}

#' Height of the water surface above the lake bed, from level observations
#'
#' Observed lake level is an elevation, so the height above the bed is the level
#' minus the lowest point of the hypsograph (the same offset `get_var()` applies
#' to compare model level with observed level).
#' @param aeme `Aeme` object.
#' @param level optional data.frame of level observations to use in place of
#'   those on `aeme` (columns `Date`, `var_aeme`, `value`).
#' @return A data.frame with `Date` and `height` (sorted by date), or `NULL` when
#'   there are no level observations or no hypsograph.
#' @noRd
obs_level_height <- function(aeme, level = NULL) {
  lvl <- if (is.null(level)) observations(aeme)$level else level
  hyp <- input(aeme)$hypsograph
  if (is.null(lvl) || nrow(lvl) == 0 || is.null(hyp) || !"elev" %in% names(hyp)) {
    return(NULL)
  }
  lvl <- lvl[lvl$var_aeme %in% "LKE_lvlwtr" & !is.na(lvl$value), , drop = FALSE]
  if (nrow(lvl) == 0) return(NULL)
  out <- data.frame(Date = as.Date(lvl$Date, tz = "UTC"),
                    height = lvl$value - min(hyp$elev, na.rm = TRUE))
  out <- out[!duplicated(out$Date), , drop = FALSE]
  out[order(out$Date), , drop = FALSE]
}

#' Interpolate a level series to dates
#'
#' Linear in time between observations, and held at the first and last value
#' outside them.
#' @param series data.frame from `obs_level_height()` or with `Date`, `height`.
#' @param dates `Date` vector.
#' @return Numeric vector of heights, `NA` if `series` is `NULL`.
#' @noRd
interp_level_height <- function(series, dates) {
  if (is.null(series) || nrow(series) == 0) return(rep(NA_real_, length(dates)))
  dates <- as.Date(dates, tz = "UTC")
  if (nrow(series) == 1) return(rep(series$height, length(dates)))
  stats::approx(as.numeric(series$Date), series$height,
                xout = as.numeric(dates), rule = 2)$y
}

#' Depth of the basin when full, from the hypsograph
#'
#' The fallback water level for placing a bottom-referenced observation when
#' there is no observed level: the same datum `get_var()` used for "bottom"
#' before it followed the water level.
#' @noRd
basin_full_depth <- function(aeme) {
  hyp <- input(aeme)$hypsograph
  if (!is.null(hyp) && all(c("depth", "elev") %in% names(hyp))) {
    hyp <- hyp[hyp$depth <= 0, , drop = FALSE]
    if (nrow(hyp)) return(max(max(hyp$elev) - hyp$elev))
  }
  d <- lake(aeme)$depth
  if (is.null(d) || !is.finite(d)) NA_real_ else d
}

#' Place bottom-referenced lake observations below the surface
#'
#' Used by [add_obs()]. In `lake`, `depth` is measured from `depth_ref` (or the
#' row's own `depth_ref`). Surface rows are left alone. For bottom rows the given
#' height goes to `height_above_bed` and `depth` becomes `level - height` for the
#' height of the water surface above the bed on that date: from level
#' observations (`level` plus those on `aeme`) interpolated in time, else the
#' basin's full depth, with a warning.
#'
#' Observations that are all surface-referenced and carry no `depth_ref` column
#' are returned untouched, so existing data and code see no new columns.
#'
#' @param aeme `Aeme` object (for its hypsograph and level observations).
#' @param lake data.frame of lake observations.
#' @param level data.frame of new level observations, or `NULL`.
#' @param depth_ref default reference for rows without their own.
#' @return `lake`, with `depth_ref` and `height_above_bed` when any row is
#'   bottom-referenced or the input had a `depth_ref` column.
#' @noRd
resolve_bottom_obs <- function(aeme, lake, level = NULL,
                               depth_ref = "surface") {

  had_col <- "depth_ref" %in% names(lake)
  ref <- if (had_col) as.character(lake$depth_ref) else rep(NA_character_, nrow(lake))
  ref[is.na(ref)] <- depth_ref
  bad <- setdiff(unique(ref), obs_depth_ref_values)
  if (length(bad)) {
    cli::cli_abort(
      c("{.field depth_ref} must be {.val surface} or {.val bottom}.",
        "x" = "Got {.val {bad}}."),
      class = "aeme_error_obs_depth_ref")
  }
  is_bottom <- ref == "bottom"

  if (!had_col && !any(is_bottom)) return(lake)
  lake$depth_ref <- ref
  if (!"height_above_bed" %in% names(lake)) lake$height_above_bed <- NA_real_

  if (!any(is_bottom)) return(normalise_obs_depth_ref(lake, require_height = TRUE))

  if (anyNA(lake$depth[is_bottom]) || any(lake$depth[is_bottom] < 0)) {
    cli::cli_abort(
      c("Heights above the bed must be non-negative numbers.",
        "i" = "For rows with {.code depth_ref = \"bottom\"}, {.field depth} is
               the height above the lake bed."),
      class = "aeme_error_obs_depth_ref")
  }

  dates <- as.Date(lake$Date[is_bottom], tz = "UTC")
  s_new <- if (!is.null(level)) obs_level_height(aeme, level = level) else NULL
  s_old <- obs_level_height(aeme)
  series <- rbind(s_new, s_old)                 # new observations win on a tie
  if (!is.null(series)) {
    series <- series[!duplicated(series$Date), , drop = FALSE]
    series <- series[order(series$Date), , drop = FALSE]
  }
  surface_height <- interp_level_height(series, dates)

  if (all(is.na(surface_height))) {
    full <- basin_full_depth(aeme)
    if (is.na(full)) {
      cli::cli_abort(
        c("Cannot place bottom-referenced observations below the surface.",
          "i" = "Add lake level observations, or give the lake a hypsograph."),
        class = "aeme_error_obs_depth_ref")
    }
    cli::cli_warn(
      c("!" = "No lake level observations: placing bottom-referenced
               observations as if the basin were full ({.val {round(full, 2)}}
               m).",
        "i" = "Their {.field depth} is an estimate. {.fn get_var} and
               {.fn ensemble_summary} compare them with the model using the
               model's own water level and {.field height_above_bed}."),
      class = "aeme_warn_obs_bottom_no_level")
    surface_height <- rep(full, length(dates))
  }

  height <- lake$depth[is_bottom]
  depth <- surface_height - height
  if (any(depth < 0, na.rm = TRUE)) {
    cli::cli_warn(
      c("!" = "{sum(depth < 0, na.rm = TRUE)} bottom-referenced observation{?s}
               {?is/are} higher above the bed than the water level.",
        "i" = "Placed at the surface."),
      class = "aeme_warn_obs_bottom_above_surface")
    depth <- pmax(depth, 0)
  }

  # an integrated sample's other end is a height too
  if ("depth_to" %in% names(lake)) {
    level_all <- rep(NA_real_, nrow(lake))
    level_all[is_bottom] <- surface_height
    idx <- which(is_bottom & !is.na(lake$depth_to))
    lake$depth_to[idx] <- pmax(level_all[idx] - lake$depth_to[idx], 0)
  }

  lake$height_above_bed[is_bottom] <- height
  lake$depth[is_bottom] <- depth
  normalise_obs_depth_ref(lake, require_height = TRUE)
}
