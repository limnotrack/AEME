#' Rebuild a DYRESM-CAEDYM `<lakename>.stg` file after an inflow/outflow edit
#'
#' The inflow *names* and the outlet *count*/*heights* are baked into the
#' `.stg` file, separately from the `.inf`/`.wdr` data files. When
#' [set_dy_cd_inflows()] / [set_dy_cd_outflows()] change either, this helper
#' re-runs `make_dy_stg()` with the new names/heights while reading
#' everything else (latitude, surface elevation, crest, bathymetry) back
#' from the existing `.stg` via `read_dy_stg()`.
#'
#' Note: `make_dy_stg()` rebuilds the inflow geometry columns
#' (entry height, half-angle, slope, drag) from its defaults -- the same
#' values [build_aeme()] itself writes -- so any hand-tuned inflow geometry
#' in the existing `.stg` is reset.
#'
#' @param path_dy filepath; the `dy_cd` model directory.
#' @param prefix character; the shared `<lakename>` prefix.
#' @param inf_names character; new inflow names, or `NULL` to keep existing.
#' @param out_names character; new outflow names, or `NULL` to keep existing
#'   (existing `.stg` files store no outlet names, so placeholders are used).
#' @param out_heights numeric; new outlet heights (m ASL), or `NULL` to keep
#'   existing.
#' @return invisibly, `NULL`.
#' @noRd
.rewrite_dy_stg <- function(path_dy, prefix, inf_names = NULL,
                            out_names = NULL, out_heights = NULL) {
  stg_file <- file.path(path_dy, paste0(prefix, ".stg"))
  stg <- read_dy_stg(stg_file)

  if (is.null(inf_names)) {
    inf_names <- stg$inflows$name
  }
  if (is.null(out_heights)) {
    out_heights <- stg$outlet_heights
  }
  if (is.null(out_names)) {
    out_names <- if (stg$n_outlets > 0) {
      paste0("outlet_", seq_len(stg$n_outlets))
    } else {
      "EMPTY"
    }
  }

  make_dy_stg(lakename = prefix,
              latitude = stg$latitude,
              bathy = stg$bathymetry,
              surfElev = stg$surface_elev,
              crest = stg$crest_elev,
              outHeights = out_heights,
              infNames = inf_names,
              outNames = out_names,
              filePath = path_dy)
  invisible(NULL)
}

#' Set inflow data for a DYRESM-CAEDYM simulation directory
#'
#' Thin, `aeme`-free wrapper around the internal inflow writer used by
#' [build_aeme()]. Writes `<lakename>.inf` into `path_dy` and, so the
#' inflow set stays consistent, rebuilds the inflow block of
#' `<lakename>.stg` (see `.rewrite_dy_stg()` note about inflow geometry
#' defaults).
#'
#' @param path_dy filepath; the `dy_cd` model directory (containing the
#' `<lakename>.stg` file).
#' @param list_inf named list of data.frames, one per inflow. Each must have
#' a `Date` column plus flow/temperature/salt columns (e.g. `HYD_flow`,
#' `HYD_temp`, `CHM_salt`) -- see [add_inflow()] for the expected schema.
#' Column names are translated to DYRESM-CAEDYM's own via
#' `rename_modelvars()`.
#' @param inf_factor numeric; scaling factor applied to all inflow flow
#' rates. Default is `1`.
#' @param update_stg logical; also rebuild the inflow block of
#' `<lakename>.stg` to match `names(list_inf)`. Default `TRUE`.
#'
#' @return invisibly, `NULL`.
#' @export
#'
#' @examples
#' \dontrun{
#' set_dy_cd_inflows(path_dy, list_inf = list(FWMT = inflow_df))
#' }
set_dy_cd_inflows <- function(path_dy, list_inf, inf_factor = 1,
                              update_stg = TRUE) {

  if (!is.list(list_inf) || is.null(names(list_inf)) ||
      any(names(list_inf) == "")) {
    cli::cli_abort("'list_inf' must be a named list of data.frames.")
  }

  prefix <- .dy_cd_prefix(path_dy)

  all_dates <- do.call(c, lapply(list_inf, \(d) as.Date(d$Date)))
  date_range <- range(all_dates, na.rm = TRUE)

  make_dy_inf(lakename = prefix, infList = list_inf, filePath = path_dy,
              date_range = date_range, inf_factor = inf_factor)

  if (isTRUE(update_stg)) {
    .rewrite_dy_stg(path_dy, prefix, inf_names = names(list_inf))
  }

  invisible(NULL)
}
