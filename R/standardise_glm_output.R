#' Standardise a raw GLM-AED output list onto AEME's common depth grid
#'
#' Interpolates every `(z, time)` variable in a `read_glm_output(raw_output
#' = TRUE)` list from GLM's own, time-varying layer structure -- captured in
#' its `z` entry (see [read_glm_output()]) -- onto a shared depth grid, the
#' same one `raw_output = FALSE` produces, without re-reading the netCDF
#' file. Also renames variables back to their AEME `var_aeme` names (where
#' [key_naming] has a translation) and re-applies AED unit-conversion
#' factors, so the result resembles `read_glm_output(raw_output =
#' FALSE)`'s output as closely as possible.
#'
#' Useful when you've already loaded raw output (e.g. to inspect native
#' units/names) and only later decide you want it on AEME's standard grid
#' too -- interpolating in place is cheaper than re-opening the netCDF file
#' and reading it again with `raw_output = FALSE`.
#'
#' `aeme_grouped_var` entries (dimensions other than `(time)`/`(z, time)`,
#' e.g. sediment-zone variables) are carried over unchanged -- they aren't
#' on a depth grid to begin with, so there's nothing to interpolate.
#'
#' @param out_raw list; an `aeme_output_raw`-classed GLM-AED output list
#'   from [read_glm_output()] with `raw_output = TRUE` (must include its
#'   `z` and `LKE_lvlwtr` entries).
#' @param depths numeric vector; depths to interpolate onto. If `NULL`
#'   (default), uses the same standardised depth-fraction grid
#'   `read_glm_output(raw_output = FALSE)` uses (see
#'   [model_layer_structure]).
#'
#' @return An `aeme_output`-classed list (see [is_aeme_output()]),
#'   structurally the same as `read_glm_output(raw_output = FALSE)`'s
#'   return value.
#' @export
#'
#' @importFrom dplyr filter mutate pull
#'
#' @examples
#' \dontrun{
#' out_raw <- read_glm_output(file = outfile, raw_output = TRUE)
#' out_std <- standardise_glm_output(out_raw)
#' plot_model_output(out_std, "HYD_temp")
#' }
standardise_glm_output <- function(out_raw, depths = NULL) {

  if (!is_aeme_output_raw(out_raw)) {
    cli::cli_abort("{.arg out_raw} must be raw GLM-AED output -- see {.fn read_glm_output} with {.code raw_output = TRUE}.")
  }
  if (!identical(attr(out_raw, "model"), "glm_aed")) {
    cli::cli_abort("{.arg out_raw} must be GLM-AED output (its {.field model} attribute is {.val {attr(out_raw, 'model')}}).")
  }
  if (is.null(out_raw[["z"]])) {
    cli::cli_abort("{.arg out_raw} has no {.field z} entry -- re-read it with a version of {.fn read_glm_output} that captures GLM's native layer heights.")
  }

  dates      <- as.Date(out_raw[["Date"]])
  lake_level <- out_raw[["LKE_lvlwtr"]]

  # Recompute layer midpoints from the raw z boundary heights at full
  # precision -- LKE_depths in raw mode is this same quantity, but already
  # rounded to 2dp for display. Named `z_mat` (not `z`) so it doesn't shadow
  # model_layer_structure's own `z` column inside the dplyr::filter() below.
  z_mat <- out_raw[["z"]]
  midpoints <- apply(z_mat, 2, \(x) x - diff(c(0, x)) / 2)
  Lmat <- matrix(lake_level, nrow = nrow(midpoints), ncol = length(lake_level),
                byrow = TRUE)
  midpoints <- Lmat - midpoints

  if (is.null(depths)) {
    max_depth <- max(lake_level, na.rm = TRUE)
    data("model_layer_structure", package = "AEME", envir = environment())
    depth_fraction <- model_layer_structure |>
      dplyr::filter(z < max_depth) |>
      dplyr::mutate(deps = z / max_depth) |>
      dplyr::pull(deps) |>
      matrix(ncol = 1)
    depth_mat <- depth_fraction %*% t(lake_level)
    out_depths <- round(depth_mat, 2)
  } else {
    out_depths <- matrix(rep(depths, length(dates)),
                         nrow = length(depths),
                         ncol = length(dates))
  }

  data("key_naming", package = "AEME", envir = environment())
  glm_to_var_aeme <- stats::setNames(key_naming$var_aeme, key_naming$glm_aed)

  # These stay as they are -- either already handled above (Date/z/
  # LKE_lvlwtr feed the interpolation itself and are set explicitly below)
  # or aren't real variables (LKE_depths/ok/reason)
  structural <- c("Date", "LKE_depths", "z", "LKE_lvlwtr", "ok", "reason")

  out_list <- list()
  for (nm in names(out_raw)) {
    if (nm %in% structural) next
    val <- out_raw[[nm]]

    key <- unname(glm_to_var_aeme[nm])
    if (is.na(key) || !nzchar(key)) key <- nm

    if (inherits(val, "aeme_grouped_var")) {
      out_list[[key]] <- val
      next
    }

    conv_idx    <- match(nm, key_naming$glm_aed)
    conv_factor <- if (!is.na(conv_idx)) key_naming$conversion_aed[conv_idx] else NA
    if (is.na(conv_factor)) conv_factor <- 1

    if (is.matrix(val)) {
      out_list[[key]] <- interp_static_grid(var = val * conv_factor,
                                            midpoints = midpoints,
                                            out_depths = out_depths)
    } else {
      out_list[[key]] <- as.vector(val) * conv_factor
    }
  }

  out_list[["Date"]]       <- dates
  out_list[["LKE_depths"]] <- out_depths
  out_list[["LKE_lvlwtr"]] <- lake_level
  out_list <- c(out_list, list(ok = TRUE, reason = NULL))

  .new_aeme_output(out_list, model = "glm_aed", raw = FALSE)
}
