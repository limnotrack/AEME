#' Set outflow data for a DYRESM-CAEDYM simulation directory
#'
#' Thin, `aeme`-free wrapper around the internal outflow writer used by
#' [build_aeme()]. Writes `<lakename>.wdr` into `path_dy` and, so the outlet
#' set stays consistent, rebuilds the outlet block of `<lakename>.stg` with
#' the supplied heights.
#'
#' DYRESM-CAEDYM's `.wdr` writer (`make_dy_wdr()`) only supports a single
#' outflow series (or the internal water-balance `outflow`/`wbal` pair), so
#' `outf` should normally hold exactly one data.frame.
#'
#' @inheritParams set_dy_cd_inflows
#' @param outf named list of data.frames, one per outflow, each with a
#' `Date` column and a `HYD_flow` column -- see [add_outflows()] for the
#' expected schema.
#' @param heights_wdr named numeric vector; outlet elevation (m ASL) for
#' each name in `outf`.
#' @param outf_factor numeric; scaling factor applied to all outflow flow
#' rates. Default is `1`.
#' @param update_stg logical; also rebuild the outlet block of
#' `<lakename>.stg` with `heights_wdr`. Default `TRUE`.
#'
#' @return invisibly, `NULL`.
#' @export
#'
#' @examples
#' \dontrun{
#' set_dy_cd_outflows(path_dy, outf = list(outflow = outflow_df),
#'                    heights_wdr = c(outflow = 12.07))
#' }
set_dy_cd_outflows <- function(path_dy, outf, heights_wdr, outf_factor = 1,
                               update_stg = TRUE) {

  if (!is.list(outf) || is.null(names(outf)) || any(names(outf) == "")) {
    cli::cli_abort("'outf' must be a named list of data.frames.")
  }
  if (is.null(names(heights_wdr)) ||
      !all(names(outf) %in% names(heights_wdr))) {
    cli::cli_abort("'heights_wdr' must be a named numeric vector covering every name in 'outf'.")
  }

  prefix <- .dy_cd_prefix(path_dy)

  make_dy_wdr(lakename = prefix, wdrData = outf, filePath = path_dy,
              outf_factor = outf_factor)

  if (isTRUE(update_stg)) {
    .rewrite_dy_stg(path_dy, prefix, out_names = names(outf),
                    out_heights = unname(heights_wdr[names(outf)]))
  }

  invisible(NULL)
}
