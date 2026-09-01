#' Normalise a lake observations data frame to the current schema
#'
#' Internal helper. The current lake observations schema requires a single
#' `depth` column (nominal / representative sampling depth, metres positive-down
#' from the surface). Older objects and CSV files use the `depth_from` /
#' `depth_to` column pair. This function converts the legacy layout to the
#' current one, keeping `depth_to` only where it genuinely records the bottom of
#' an integrated sample.
#'
#' The function is idempotent: a data frame that already has a `depth` column is
#' returned unchanged (bar coercion of `depth` to numeric).
#'
#' @param lake data frame of lake observations, or `NULL`.
#' @param warn logical; emit a one-time deprecation warning when a legacy layout
#'   is converted. Default `TRUE`.
#'
#' @return A data frame with a `depth` column (plus optional `depth_to` / `sd`),
#'   or `NULL` if `lake` is `NULL`.
#' @keywords internal
#' @noRd
normalise_lake_obs <- function(lake, warn = TRUE) {
  if (is.null(lake)) return(NULL)
  if (!is.data.frame(lake)) return(lake)

  nms <- names(lake)

  # Already on the current schema.
  if ("depth" %in% nms) {
    lake$depth <- as.numeric(lake$depth)
    if ("depth_to" %in% nms) lake$depth_to <- as.numeric(lake$depth_to)
    if ("sd" %in% nms) lake$sd <- as.numeric(lake$sd)
    return(lake)
  }

  # Nothing depth-like to work with - leave it for the caller's column check.
  if (!any(c("depth_from", "depth_mid") %in% nms)) return(lake)

  if ("depth_from" %in% nms && "depth_to" %in% nms) {
    depth_from <- as.numeric(lake$depth_from)
    depth_to <- as.numeric(lake$depth_to)
    lake$depth <- (depth_from + depth_to) / 2
    # Retain depth_to only for genuine intervals.
    interval <- !is.na(depth_from) & !is.na(depth_to) & depth_from != depth_to
    if (any(interval)) {
      lake$depth_to <- ifelse(interval, depth_to, NA_real_)
    } else {
      lake$depth_to <- NULL
    }
    lake$depth_from <- NULL
  } else if ("depth_from" %in% nms) {
    lake$depth <- as.numeric(lake$depth_from)
    lake$depth_from <- NULL
  } else {
    # only depth_mid
    lake$depth <- as.numeric(lake$depth_mid)
  }
  lake$depth_mid <- NULL

  if ("sd" %in% names(lake)) lake$sd <- as.numeric(lake$sd)

  if (warn) {
    cli::cli_warn(
      c("!" = "Lake observations use the legacy {.field depth_from} /
        {.field depth_to} columns.",
        "i" = "These have been collapsed to a single {.field depth} column
        (interval midpoint). Update your data to the current schema
        ({.val {get_obs_column_names()}}); {.field depth_to} and {.field sd} are
        optional."),
      class = "aeme_warn_obs_legacy_depth",
      .frequency = "once",
      .frequency_id = "aeme_warn_obs_legacy_depth"
    )
  }

  lake
}
