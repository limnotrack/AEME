#' Read from a Parquet store written by write_aeme_parquet()
#'
#' Queries the Hive-partitioned Parquet store produced by
#' \code{\link{write_aeme_parquet}}, applying any of `lake_id`, `model`,
#' `var_sim`, and `date_range` as filters *before* the data is read into
#' memory -- partition pruning (`lake_id`/`model`) skips whole files, and the
#' `var_sim`/`date_range` filters are pushed down into the Parquet scan by
#' `arrow`, so filtering to e.g. one variable for one lake never touches the
#' other lakes/variables on disk.
#'
#' `what = "output_summary"` only has data for lakes with more than one
#' ensemble member (see \code{\link{write_aeme_parquet}}); requesting it
#' for a single-ensemble lake returns 0 rows for that lake, with a warning
#' pointing at `output_raw` instead.
#'
#' @param out_root character; root directory of the parquet store (as
#'   passed to \code{\link{write_aeme_parquet}}).
#' @param what character; which part of the store to read: `"output_summary"`
#'   (default; ensemble-collapsed `mean`/`sd`/quantiles per
#'   `date`/`var_sim`/`depth`), `"output_raw"` (every ensemble member), or
#'   `"lakes"` (the one-row-per-lake dimension table).
#' @param lake_id character vector; restrict to these lake ids. Default: all
#'   lakes in the store. Applied to `what = "lakes"` too.
#' @param model character vector; restrict to these model codes (see
#'   \code{\link{list_models}}). Ignored for `what = "lakes"`.
#' @param var_sim character vector; restrict to these `var_sim` codes.
#'   Ignored for `what = "lakes"`.
#' @param date_range length-2 Date/POSIXct/character vector; restrict to
#'   `date >= date_range[1] & date <= date_range[2]`. Ignored for
#'   `what = "lakes"`.
#' @param collect logical; if `TRUE` (default), materialise the result as a
#'   tibble. If `FALSE`, return the lazy `arrow` query so further `dplyr`
#'   verbs (e.g. an additional `group_by()`/`summarise()`) can be chained
#'   -- and pushed down where possible -- before `dplyr::collect()`.
#'
#' @return A tibble, or a lazy `arrow` `Dataset`/query object if
#'   `collect = FALSE`.
#' @export
#'
#' @importFrom dplyr filter collect
#' @importFrom rlang arg_match check_installed
#'
#' @examples
#' \dontrun{
#' out_root <- file.path(tempdir(), "dashboard")
#'
#' # every lake's ensemble-summary surface temperature
#' surf_temp <- read_aeme_parquet(out_root, var_sim = "HYD_temp") |>
#'   dplyr::filter(depth == min(depth))
#'
#' # full ensemble spread for one lake, for a spaghetti plot
#' ens <- read_aeme_parquet(out_root, what = "output_raw",
#'                            lake_id = "LID45819", var_sim = "HYD_temp")
#'
#' lakes <- read_aeme_parquet(out_root, what = "lakes")
#' }
read_aeme_parquet <- function(out_root,
                                what = c("output_summary", "output_raw", "lakes"),
                                lake_id = NULL, model = NULL, var_sim = NULL,
                                date_range = NULL, collect = TRUE) {

  rlang::check_installed("arrow")
  what <- rlang::arg_match(what)

  if (what == "lakes") {
    lakes_file <- file.path(out_root, "lakes.parquet")
    if (!file.exists(lakes_file)) {
      cli::cli_abort(
        c("No {.file lakes.parquet} found in {.path {out_root}}.",
          "i" = "Has {.fn write_aeme_parquet} been run yet?"),
        class = "aeme_error_parquet_not_found"
      )
    }
    out <- arrow::read_parquet(lakes_file)
    if (!is.null(lake_id)) {
      out <- dplyr::filter(out, .data$lake_id %in% .env$lake_id)
    }
    return(as.data.frame(out))
  }

  ds_dir <- file.path(out_root, what)
  if (!dir.exists(ds_dir)) {
    if (what == "output_summary") {
      cli::cli_abort(
        c("No {.path {ds_dir}} directory found in {.path {out_root}}.",
          "i" = "{.fn write_aeme_parquet} only writes {.val output_summary}
                for lakes with more than one ensemble member -- if every
                lake in this store has a single member, query
                {.val output_raw} instead."),
        class = "aeme_error_parquet_not_found"
      )
    }
    cli::cli_abort(
      c("No {.path {ds_dir}} directory found in {.path {out_root}}.",
        "i" = "Has {.fn write_aeme_parquet} been run yet?"),
      class = "aeme_error_parquet_not_found"
    )
  }

  # write_aeme_parquet() skips output_summary for single-member lakes
  # (see its @details) -- flag any explicitly-requested lake that will
  # therefore be silently absent from the result, rather than leaving the
  # caller to wonder why a lake_id came back with 0 rows.
  if (what == "output_summary" && !is.null(lake_id)) {
    lakes_file <- file.path(out_root, "lakes.parquet")
    if (file.exists(lakes_file)) {
      lakes_tbl <- as.data.frame(arrow::read_parquet(lakes_file))
      single_ens <- lakes_tbl$lake_id[lakes_tbl$lake_id %in% lake_id &
                                       lakes_tbl$n_ens <= 1]
      if (length(single_ens) > 0) {
        cli::cli_warn(
          c("!" = "Requested lake(s) with only one ensemble member have no
                  {.val output_summary} data: {.val {single_ens}}.",
            "i" = "Query {.val output_raw} for these lakes instead."),
          class = "aeme_warn_no_summary_single_ens"
        )
      }
    }
  }

  ds <- arrow::open_dataset(ds_dir)

  if (!is.null(lake_id)) ds <- dplyr::filter(ds, .data$lake_id %in% .env$lake_id)
  if (!is.null(model))   ds <- dplyr::filter(ds, .data$model %in% .env$model)
  if (!is.null(var_sim)) ds <- dplyr::filter(ds, .data$var_sim %in% .env$var_sim)
  if (!is.null(date_range)) {
    ds <- dplyr::filter(ds, .data$date >= .env$date_range[1],
                        .data$date <= .env$date_range[2])
  }

  if (collect) dplyr::collect(ds) else ds
}
