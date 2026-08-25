#' Set outflow data for a GOTM-WET simulation directory
#'
#' Thin, `aeme`-free wrapper around the internal outflow writer used by
#' [build_aeme()]. Writes `inputs/outf_<name>.dat` per outflow into
#' `path_gotm`, and updates the `streams` block of `gotm.yaml` to point at
#' them. Existing stream entries not named in `outf` are left untouched.
#'
#' @inheritParams set_gotm_inflows
#' @param outf named list of data.frames, one per outflow, each with a
#' `Date` column and a `HYD_flow` column -- see [add_outflows()] for the
#' expected schema.
#' @param outf_factor numeric; scaling factor applied to all outflow flow
#' rates. Default is `1`.
#'
#' @return invisibly, the updated yaml list
#' @export
#'
#' @examples
#' \dontrun{
#' set_gotm_outflows(path_gotm, outf = list(outlet_1 = outflow_df))
#' }

set_gotm_outflows <- function(path_gotm, outf, outf_factor = 1,
                              yaml_file = file.path(path_gotm, "gotm.yaml")) {

  if (!is.list(outf) || is.null(names(outf)) || any(names(outf) == "")) {
    cli::cli_abort("'outf' must be a named list of data.frames.")
  }

  gotm <- yaml::read_yaml(yaml_file)

  make_wdr_gotm(outf = outf, path_gotm = path_gotm, outf_factor = outf_factor)

  # make_wdr_gotm() only writes the data files -- the streams block itself
  # is assembled here, mirroring make_yaml_gotm()'s own (build-time) logic
  for (n in names(outf)) {
    gotm[["streams"]][[n]] <- list(
      method = 1, zu = 0, zl = -1,
      flow = list(method = 2, constant_value = 0,
                 file = paste0("inputs/outf_", n, ".dat"),
                 column = 1, scale_factor = 1, offset = 0)
    )
  }
  write_yaml(gotm, yaml_file)

  invisible(gotm)
}
