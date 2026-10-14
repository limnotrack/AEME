#' Set outflow data for a Simstrat-AED2 simulation directory
#'
#' Thin, `aeme`-free wrapper around the internal outflow writer used by
#' [build_aeme()]. Writes a combined `Qout.dat` into `path_simstrat`. All
#' outflows are summed into a single series at a single representative
#' withdrawal elevation, matching `make_wdr_simstrat()`'s existing
#' behaviour.
#'
#' @inheritParams set_simstrat_inflows
#' @param outf named list of data.frames, one per outflow, each with a
#' `Date` column and a `HYD_flow` column -- see [add_outflows()] for the
#' expected schema.
#' @param heights_wdr named numeric vector; withdrawal elevation (m,
#' absolute -- the same datum as `hyps$elev`/`surface_elev`) for each name
#' in `outf`.
#' @param surface_elev numeric; the lake surface elevation (m) that this
#' Simstrat-AED2 configuration was built with -- the zero-point for
#' `Bathymetry.dat`/`InitialConditions.dat`/inflow-outflow depths (see
#' `make_stg_simstrat()`). Unlike GLM's nml, Simstrat's own files only store
#' depths already relative to this elevation, so it cannot be recovered from
#' `path_simstrat` alone and must be supplied.
#' @param outf_factor numeric; scaling factor applied to all outflow flow
#' rates. Default is `1`.
#'
#' @return invisibly, `NULL`
#' @export
#'
#' @examples
#' \dontrun{
#' set_simstrat_outflows(path_simstrat, outf = list(outlet_1 = outflow_df),
#'                       heights_wdr = c(outlet_1 = 10.5), surface_elev = 13.07)
#' }

set_simstrat_outflows <- function(path_simstrat, outf, heights_wdr,
                                  surface_elev, outf_factor = 1,
                                  ref_year = NULL,
                                  par_file = file.path(path_simstrat, "simstrat.par")) {

  if (!is.list(outf) || is.null(names(outf)) || any(names(outf) == "")) {
    cli::cli_abort("'outf' must be a named list of data.frames.")
  }
  if (is.null(names(heights_wdr)) ||
      !all(names(outf) %in% names(heights_wdr))) {
    cli::cli_abort("'heights_wdr' must be a named numeric vector covering every name in 'outf'.")
  }

  if (is.null(ref_year)) {
    par <- jsonlite::fromJSON(par_file, simplifyVector = FALSE)
    ref_year <- as.integer(par[["Simulation"]][["Reference year"]])
  }

  make_wdr_simstrat(outf = outf, heights_wdr = heights_wdr,
                    path_simstrat = path_simstrat, surface_elev = surface_elev,
                    outf_factor = outf_factor, ref_year = ref_year)

  invisible(NULL)
}
