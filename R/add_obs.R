#' Add observations to Aeme object
#'
#' @inheritParams build_aeme
#' @param lake data frame with required columns "Date", "var_aeme", "depth" and
#' "value", and optional columns "depth_to" (bottom of an integrated sample),
#' "sd" (measurement standard deviation, in the variable's units) and
#' "depth_ref" (see `depth_ref`). The legacy "depth_from" / "depth_to" column
#' pair is accepted and collapsed to "depth" (with a one-time deprecation
#' warning). "Date" may be a `Date`, `POSIXct` or date/datetime string; it is
#' stored as a UTC `POSIXct` with daily rows anchored at 12:00:00. If NULL, no
#' observations are added.
#' @param level data frame with columns "Date", "var_aeme" and "value". "Date"
#' is handled as for `lake`. If NULL, no observations are added.
#' @param depth_ref character; what "depth" in `lake` is measured from.
#' `"surface"` (default): metres below the water surface. `"bottom"`: metres
#' above the lake bed, for observations recorded as a height off the bottom (a
#' logger 1 m above the sediment). A "depth_ref" column in `lake`, with values
#' `"surface"` or `"bottom"`, sets this per row and takes precedence; `depth_ref`
#' applies to rows without one.
#'
#' @details
#' Stored observations always have `depth` in metres below the surface, so
#' everything that reads it keeps working. For a bottom-referenced row the height
#' you give is kept in `height_above_bed` (with `depth_ref = "bottom"`), and
#' `depth` is worked out as `level - height`, where `level` is the height of the
#' water surface above the bed on that date: from the lake level observations
#' (those in `level`, or already on the object), interpolated in time; failing
#' that, the full depth of the basin, with a warning because it ignores how far
#' the real level was from full. That `depth` is an estimate. [get_var()] with
#' `use_obs = TRUE` and [ensemble_summary()] compare bottom-referenced
#' observations with the model at `height_above_bed` using the model's own water
#' level, which is what "1 m above the bed" means.
#'
#' @returns Aeme object with observations added
#' @export
#'

add_obs <- function(aeme, lake = NULL, level = NULL,
                    depth_ref = c("surface", "bottom")) {

  depth_ref <- rlang::arg_match(depth_ref)

  # Check if aeme is a Aeme object
  aeme <- check_aeme(aeme)
  obs <- observations(aeme)

  if (!is.null(lake)) {

    if (!is.data.frame(lake)) {
      stop("lake must be a data frame with columns 'Date', 'var_aeme', 'depth' and 'value'")
    }

    # Accept the legacy depth_from / depth_to layout
    lake <- normalise_lake_obs(lake)

    if (!all(get_obs_column_names() %in% colnames(lake))) {
      stop("lake must be a data frame with columns 'Date', 'var_aeme', 'depth' and 'value'")
    }

    # Store Date as noon-anchored UTC POSIXct (daily rows at 12:00:00) so it
    # matches obs already on the object and a sub-daily model axis.
    if (!inherits(lake$Date, c("POSIXct", "Date"))) {
      cli::cli_warn(
        c("!" = "{.arg lake$Date} is not {.cls POSIXct} or {.cls Date}.",
          "i" = "Parsing it as {.val UTC} and anchoring daily rows at 12:00:00."),
        class = "aeme_warn_obs_date_coerced"
      )
    }
    lake$Date <- .as_obs_datetime(lake$Date)

    # Observations recorded as a height above the lake bed are placed below the
    # surface here; surface-referenced observations are left as they are
    lake <- resolve_bottom_obs(aeme, lake, level = level, depth_ref = depth_ref)

    orig_data <- obs$lake

    # Combine existing and new lake data, avoiding duplicates
    if (!is.null(orig_data)) {
      combined_data <- dplyr::bind_rows(orig_data, lake) |>
        dplyr::distinct(Date, var_aeme, depth,
                        dplyr::across(dplyr::any_of("depth_to")),
                        .keep_all = TRUE) |>
        dplyr::arrange(Date, var_aeme, depth)
      # rows added before depth_ref existed are surface-referenced
      obs$lake <- normalise_obs_depth_ref(combined_data, require_height = TRUE)
    } else {
      obs$lake <- lake
    }

  }
  
  if (!is.null(level)) {

    # Check if level is a data frame with columns "Date", "var_aeme" and "value"
    if (!is.data.frame(level) || !all(c("Date", "var_aeme", "value") %in% colnames(level))) {
      stop("level must be a data frame with columns 'Date', 'var_aeme' and 'value'")
    }

    if (!inherits(level$Date, c("POSIXct", "Date"))) {
      cli::cli_warn(
        c("!" = "{.arg level$Date} is not {.cls POSIXct} or {.cls Date}.",
          "i" = "Parsing it as {.val UTC} and anchoring daily rows at 12:00:00."),
        class = "aeme_warn_obs_date_coerced"
      )
    }
    level$Date <- .as_obs_datetime(level$Date)

    orig_data <- obs$level
    # Combine existing and new level data, avoiding duplicates
    if (!is.null(orig_data)) {
      combined_data <- dplyr::bind_rows(orig_data, level) |>
        dplyr::distinct(Date, var_aeme, .keep_all = TRUE) |> 
        dplyr::arrange(Date, var_aeme)
      obs$level <- combined_data
    } else {
      obs$level <- level
    }

  }

  observations(aeme) <- obs
  return(aeme)
}
