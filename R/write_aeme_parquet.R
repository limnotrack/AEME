#' Export an Aeme object's model output to a partitioned Parquet store
#'
#' Writes the long-format model output already loaded onto `aeme` (see
#' \code{\link{load_output}}) into a Hive-partitioned Parquet store suitable
#' for querying across many lakes at once (e.g. with DuckDB), plus updates a
#' single `lakes.parquet` dimension table describing every lake exported into
#' `out_root` so far.
#'
#' Two output tiers are written per model, partitioned as
#' `<tier>/lake_id=<id>/model=<model>/part-0.parquet`:
#' \itemize{
#'   \item `output_raw`: every ensemble member's full time series
#'   (`date`, `ens`, `var_sim`, `depth`, `value`) -- for drill-down into a
#'   single lake (e.g. a spaghetti plot across ensemble members).
#'   \item `output_summary`: `value` collapsed across the ensemble dimension
#'   at each `date`/`var_sim`/`depth` into `mean`, `sd`, `n_ens`, and the
#'   requested quantiles (named `q05`, `q50`, ... from `quantiles`) -- for
#'   overview views that scan many lakes at once, so the ensemble collapse
#'   doesn't need recomputing on every load.
#' }
#' `output_summary` is only written when `aeme` has more than one ensemble
#' member: with a single member, collapsing across `ens` is a no-op that
#' would just re-store the same values under 8 columns (`mean`, `sd`,
#' `n_ens`, 5 quantile columns) instead of 1 (`value`) -- pure overhead, no
#' new information. Readers should check a lake's `n_ens` in `lakes.parquet`
#' (see \code{\link{read_aeme_parquet}}) and fall back to `output_raw` for
#' any lake where it's `1`.
#'
#' Re-running this for one lake only rewrites that lake's own partition
#' directories and its one row in `lakes.parquet`; other lakes already
#' exported into `out_root` are untouched. If a lake's ensemble count
#' changes between exports (e.g. it grows from 1 to many members), any
#' stale `output_summary` partition for that lake/model is removed or
#' (re)written to match.
#'
#' @param aeme Aeme object with output loaded (see \code{\link{load_output}}).
#' @param out_root character; root directory of the store (created
#'   if missing).
#' @param model character vector of model codes to export (see
#'   \code{\link{list_models}}). Default: every model with output on `aeme`.
#' @param quantiles numeric vector of quantiles (in `[0, 1]`) for the
#'   ensemble summary tier. Default `c(0.05, 0.25, 0.5, 0.75, 0.95)`.
#' @param compression character; Parquet compression codec, passed to
#'   \code{arrow::write_parquet()}. Default `"gzip"` -- slower to write than
#'   `arrow`'s own default (`"snappy"`) but meaningfully smaller on disk for
#'   this kind of repetitive numeric time series, which matters more here
#'   than write speed since a store is written occasionally and
#'   read often. Use `"snappy"` instead if write/decode speed matters more
#'   than size for your case, or `"zstd"` for a middle ground.
#'
#' @return (Invisibly) a list with `lake_row` (the dimension-table row just
#'   written), `raw_files`, and `summary_files` (paths written; empty if the
#'   summary tier was skipped because `aeme` only has one ensemble member).
#' @export
#'
#' @importFrom dplyr bind_rows mutate select group_by summarise n filter
#' @importFrom tidyr unnest_wider
#' @importFrom tibble tibble
#' @importFrom stats quantile sd
#' @importFrom rlang check_installed
#'
#' @examples
#' \dontrun{
#' aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
#' aeme <- readRDS(aeme_file)
#' write_aeme_parquet(aeme, out_root = file.path(tempdir(), "parquet"))
#' }
write_aeme_parquet <- function(aeme, out_root, model,
                                 quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95),
                                 compression = "gzip") {

  rlang::check_installed("arrow")

  aeme <- check_aeme(aeme)
  if (missing(model)) {
    model <- list_models(aeme)
  } else {
    model <- check_model(model = model)
  }

  lke <- lake(aeme)
  tme <- time(aeme)
  outp <- output(aeme)
  n_members <- outp$n_members
  if (is.null(n_members) || n_members == 0) {
    cli::cli_abort(
      c("{.arg aeme} has no output loaded.",
        "i" = "Run {.fn load_output} on it first."),
      class = "aeme_error_no_output"
    )
  }

  dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

  q_names <- paste0("q", formatC(round(quantiles * 100), width = 2, flag = "0"))

  raw_files <- character(0)
  summary_files <- character(0)
  models_written <- character(0)

  for (m in model) {

    out_vars <- get_output_vars(aeme = aeme, model = m)
    if (length(out_vars) == 0) next

    ens_df <- lapply(seq_len(n_members), \(ens) {
      lapply(out_vars, \(v) {
        tryCatch({
          get_var(aeme = aeme, model = m, var_sim = v, return_df = TRUE,
                  ens_n = ens) |>
            dplyr::mutate(value = round(value, 4)) |>
            dplyr::select(date = Date, var_sim, depth, value) |>
            dplyr::mutate(ens = ens)
        }, error = function(e) NULL)
      }) |>
        dplyr::bind_rows()
    }) |>
      dplyr::bind_rows()

    if (nrow(ens_df) == 0) next
    models_written <- c(models_written, m)

    # --- Raw tier: every ensemble member ------------------------------
    raw_dir <- file.path(out_root, "output_raw",
                         paste0("lake_id=", lke$id),
                         paste0("model=", m))
    dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
    raw_file <- file.path(raw_dir, "part-0.parquet")
    arrow::write_parquet(ens_df, raw_file, compression = compression)
    raw_files <- c(raw_files, raw_file)

    # --- Summary tier: collapsed across the ensemble dimension --------
    # Skipped when there's only one ensemble member -- see @details.
    summ_dir <- file.path(out_root, "output_summary",
                          paste0("lake_id=", lke$id),
                          paste0("model=", m))
    if (n_members > 1) {
      summ_df <- ens_df |>
        dplyr::group_by(date, var_sim, depth) |>
        dplyr::summarise(
          mean  = mean(value, na.rm = TRUE),
          sd    = stats::sd(value, na.rm = TRUE),
          n_ens = dplyr::n(),
          q     = list(stats::setNames(
            as.numeric(stats::quantile(value, probs = quantiles, na.rm = TRUE)),
            q_names)),
          .groups = "drop"
        ) |>
        tidyr::unnest_wider(q)

      dir.create(summ_dir, recursive = TRUE, showWarnings = FALSE)
      summ_file <- file.path(summ_dir, "part-0.parquet")
      arrow::write_parquet(summ_df, summ_file, compression = compression)
      summary_files <- c(summary_files, summ_file)
    } else if (dir.exists(summ_dir)) {
      # a previous export of this lake/model had n_ens > 1 -- remove the
      # now-stale summary partition rather than leaving outdated data behind.
      # gc() first: a live arrow read elsewhere in the session (e.g. a prior
      # read_aeme_parquet() call) can leave one of these files
      # memory-mapped on Windows, which then blocks its own deletion.
      gc(verbose = FALSE)
      unlink(summ_dir, recursive = TRUE)
      if (dir.exists(summ_dir)) {
        cli::cli_warn(
          c("!" = "Could not remove the stale {.path {summ_dir}} directory.",
            "i" = "It may be open elsewhere (e.g. a live {.fn read_aeme_parquet}
                  result) -- remove it manually once nothing holds it open."),
          class = "aeme_warn_stale_summary_not_removed"
        )
      } else {
        # tidy up the now-empty lake_id=<id> parent left behind under
        # output_summary/ once its last model= subdirectory is gone.
        lake_summ_dir <- dirname(summ_dir)
        if (dir.exists(lake_summ_dir) &&
            length(list.files(lake_summ_dir)) == 0) {
          unlink(lake_summ_dir, recursive = TRUE)
        }
      }
    }
  }

  # --- Dimension table: one row per lake, replaced on re-export --------
  lake_row <- tibble::tibble(
    lake_id   = lke$id,
    name      = lke$name,
    latitude  = lke$latitude,
    longitude = lke$longitude,
    elevation = lke$elevation,
    depth     = lke$depth,
    area      = lke$area,
    start     = as.character(tme$start),
    stop      = as.character(tme$stop),
    models    = paste(models_written, collapse = ","),
    n_ens     = n_members
  )

  lakes_file <- file.path(out_root, "lakes.parquet")
  lakes_df <- if (file.exists(lakes_file)) {
    existing <- as.data.frame(arrow::read_parquet(lakes_file))
    # arrow's parquet reader can leave the file memory-mapped until R's GC
    # runs (a Windows-specific quirk -- a plain overwrite can then fail with
    # "operation cannot be performed on a file with a user-mapped section
    # open"). Forcing a copy out of Arrow's memory pool and collecting
    # releases the mapping before we try to rewrite the same path below.
    gc(verbose = FALSE)
    dplyr::filter(existing, lake_id != lke$id) |>
      dplyr::bind_rows(lake_row)
  } else {
    lake_row
  }
  arrow::write_parquet(lakes_df, lakes_file, compression = compression)

  return(invisible(list(lake_row = lake_row, raw_files = raw_files,
                        summary_files = summary_files)))
}
