#' Set outflow data for a GLM-AED simulation directory
#'
#' Thin, `aeme`-free wrapper around the internal outflow writer used by
#' [build_aeme()]. Writes one boundary-condition csv per outflow into
#' `path_glm/bcs` and updates the `&outflow` block of the GLM nml file.
#' Basin geometry (`bathy`, `dims_lake`) needed to size the outlets is read
#' from the existing `&morphometry` block by default, so an existing
#' GLM-AED configuration can have its outflows edited without a lake
#' shapefile.
#'
#' @inheritParams set_glm_inflows
#' @param outf named list of data.frames, one per outflow, each with a
#' `Date` column and a flow column (`HYD_flow`, or `outflow` for a
#' `"wbal"` water-balance outflow) -- see [add_outflows()] for the expected
#' schema.
#' @param heights_wdr named numeric vector; outlet elevation (m) for each
#' name in `outf`.
#' @param outlet_type named numeric vector; GLM outlet type per outflow
#' (see the GLM manual). Defaults to `1` (fixed height) for every outflow.
#' @param flt_off_sw named logical vector; floating offtake switch per
#' outflow. Defaults to `FALSE` for every outflow.
#' @param bathy data.frame with `elev`/`area` columns describing the lake
#' hypsograph, used to size the outlet. Defaults to the `H`/`A` arrays
#' already in the GLM nml's `&morphometry` block.
#' @param dims_lake numeric vector of length 2, `c(basin_length,
#' basin_width)` at the crest. Defaults to the `bsn_len`/`bsn_wid` values
#' already in the GLM nml's `&morphometry` block.
#' @param wdr_factor numeric; scaling factor applied to all outflow flow
#' rates. Default is `1`.
#'
#' @return invisibly, the updated nml object
#' @export
#'
#' @examples
#' \dontrun{
#' set_glm_outflows(path_glm, outf = list(outlet_1 = outflow_df),
#'                  heights_wdr = c(outlet_1 = -2.5))
#' }

set_glm_outflows <- function(path_glm, outf, heights_wdr,
                             outlet_type = NULL, flt_off_sw = NULL,
                             bathy = NULL, dims_lake = NULL,
                             wdr_factor = 1,
                             glm_file = find_glm_nml(path_glm)) {

  if (!is.list(outf) || is.null(names(outf)) || any(names(outf) == "")) {
    cli::cli_abort("'outf' must be a named list of data.frames.")
  }
  if (is.null(names(heights_wdr)) ||
      !all(names(outf) %in% names(heights_wdr))) {
    cli::cli_abort("'heights_wdr' must be a named numeric vector covering every name in 'outf'.")
  }

  glm_nml <- read_nml(glm_file)

  if (is.null(bathy)) {
    bathy <- data.frame(elev = get_nml_value(glm_nml, "H"),
                        area = get_nml_value(glm_nml, "A"))
  }
  if (is.null(dims_lake)) {
    dims_lake <- c(get_nml_value(glm_nml, "bsn_len"),
                   get_nml_value(glm_nml, "bsn_wid"))
  }
  if (is.null(outlet_type)) {
    outlet_type <- stats::setNames(rep(1, length(outf)), names(outf))
  }
  if (is.null(flt_off_sw)) {
    flt_off_sw <- stats::setNames(rep(FALSE, length(outf)), names(outf))
  }

  glm_nml <- make_wdr_glm(outf = outf, heights_wdr = heights_wdr,
                          outlet_type = outlet_type, flt_off_sw = flt_off_sw,
                          bathy = bathy, dims_lake = dims_lake,
                          wdr_factor = wdr_factor, update_nml = TRUE,
                          glm_nml = glm_nml, path_glm = path_glm)
  write_nml(glm_nml, file = glm_file)

  invisible(glm_nml)
}
