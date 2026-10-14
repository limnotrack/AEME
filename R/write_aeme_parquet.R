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
#'
#' ## Depth-resolved variables
#'
#' AEME stores depth-resolved output on a grid of fractions of the water depth
#' scaled to the lake level on each day, so the absolute depths differ between
#' ensemble members whose water level differs (and drift as a member's level
#' moves). `output_raw` keeps that grid as it is. Collapsing across members *at
#' those depths* would leave most groups with one or two members, so by default
#' `output_summary` is only really useful for variables with no depth dimension.
#' Supply `depths` to compute the summary of depth-resolved variables at fixed
#' depths instead: each member's profile is linearly interpolated to the
#' requested depths on every date before the members are collapsed. A depth that
#' falls outside a member's water column on a date (deeper than the lake, or
#' above the surface) is left out for that member, so `n_ens` says how many
#' members contributed.
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
#' @param depths numeric vector of depths (m) at which to summarise
#'   depth-resolved variables in `output_summary`, referenced to `depth_ref`.
#'   `NULL` (default) summarises at each member's own depths -- see Details.
#'   Only the summary tier is affected; `output_raw` always holds the stored
#'   depths, in metres below the surface.
#' @param depth_ref character; what `depths` is measured from. `"surface"`
#'   (default): metres below the water surface. `"bottom"`: metres above the
#'   lake bed, so `1` is the water 1 m above the bottom, found from each
#'   member's water level (`LKE_lvlwtr`) on each date. Requires `depths`.
#'   Stored in the `depth_ref` column of `output_summary`.
#'
#' @return (Invisibly) a list with `lake_row` (the dimension-table row just
#'   written), `raw_files`, and `summary_files` (paths written; empty if the
#'   summary tier was skipped because `aeme` only has one ensemble member).
#' @export
#'
#' @importFrom dplyr bind_rows mutate select group_by summarise n filter
#' @importFrom tibble tibble
#' @importFrom stats quantile sd
#' @importFrom rlang check_installed
#'
#' @examples
#' \dontrun{
#' aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
#' aeme <- readRDS(aeme_file)
#' write_aeme_parquet(aeme, out_root = file.path(tempdir(), "parquet"))
#'
#' # Summarise depth-resolved variables 1 m and 5 m below the surface, and 1 m
#' # above the lake bed
#' write_aeme_parquet(aeme, out_root = file.path(tempdir(), "parquet_depths"),
#'                    depths = c(1, 5))
#' write_aeme_parquet(aeme, out_root = file.path(tempdir(), "parquet_bottom"),
#'                    depths = 1, depth_ref = "bottom")
#' }
write_aeme_parquet <- function(aeme, out_root, model,
                                 quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95),
                                 compression = "gzip",
                                 depths = NULL,
                                 depth_ref = c("surface", "bottom")) {

  rlang::check_installed("arrow")
  depth_ref <- rlang::arg_match(depth_ref)
  if (!is.null(depths)) {
    if (!is.numeric(depths) || length(depths) == 0 || anyNA(depths) ||
        any(!is.finite(depths)) || any(depths < 0)) {
      cli::cli_abort("{.arg depths} must be non-negative numbers (metres).")
    }
    depths <- sort(unique(depths))
  } else if (depth_ref == "bottom") {
    cli::cli_abort(c("{.code depth_ref = \"bottom\"} needs {.arg depths}.",
                     "i" = "Without {.arg depths} the summary uses the stored
                            depths, which are metres below the surface."))
  }

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
      summ_in <- if (is.null(depths)) ens_df else
        interpolate_to_depths(ens_df, depths = depths, depth_ref = depth_ref)
      summ_df <- summarise_ensemble(summ_in, quantiles)
      # what `depth` is measured from; scalar variables have no depth
      summ_df <- tibble::add_column(
        summ_df,
        depth_ref = ifelse(is.na(summ_df$depth), NA_character_,
                           if (is.null(depths)) "surface" else depth_ref),
        .after = "depth")

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

#' Collapse an ensemble across members at each date, variable and depth
#'
#' Computes the same table as grouping `df` by `date`, `var_sim` and `depth`
#' and summarising `value` with `mean()`, `sd()`, `n()` and
#' `quantile(type = 7)` (all with `na.rm = TRUE`), but without a list-column
#' per group. Depth-resolved output is stored on a grid scaled to each member's
#' water level, so depths differ between members whose level differs; a
#' realistic ensemble has around a million groups, most with one to a few
#' members, and building a list for each of them through `dplyr::summarise()` took
#' over a minute, against a second or two here. Group ids come from
#' `dplyr::group_indices()` and the statistics from `rowsum()` and a sort within
#' group.
#'
#' @param df data.frame with columns `date`, `var_sim`, `depth` and `value`
#'   (any other columns are ignored).
#' @param quantiles numeric; quantile probabilities in `[0, 1]`.
#'
#' @return A tibble with the group keys (`date`, `var_sim`, `depth`, sorted as
#'   `dplyr::group_by()` sorts them), `mean`, `sd`, `n_ens` (rows in the group,
#'   including missing values) and one column per quantile, named `q05`, `q50`,
#'   ... as in [write_aeme_parquet()]. Groups with no non-missing value have
#'   `NaN` mean and `NA` for the rest; `sd` is `NA` for a single value.
#' @noRd
summarise_ensemble <- function(df, quantiles) {

  q_names <- paste0("q", formatC(round(quantiles * 100), width = 2, flag = "0"))

  grp <- dplyr::group_by(df, dplyr::across(dplyr::all_of(
    c("date", "var_sim", "depth"))))
  keys <- dplyr::group_keys(grp)
  g <- dplyr::group_indices(grp)
  n_groups <- nrow(keys)

  ok <- !is.na(df$value)
  gv <- g[ok]
  v <- as.numeric(df$value[ok])
  n_ok <- tabulate(gv, nbins = n_groups)

  # Per-group sum, as a vector indexed by group id (rowsum() drops empty groups)
  group_sum <- function(x) {
    r <- rowsum(x, gv)
    out <- numeric(n_groups)
    out[as.integer(rownames(r))] <- r[, 1]
    out
  }
  mean_v <- group_sum(v) / n_ok
  ss <- group_sum((v - mean_v[gv])^2)
  sd_v <- ifelse(n_ok > 1, sqrt(ss / (n_ok - 1)), NA_real_)

  # Type-7 quantiles, read from the values sorted within each group
  vs <- v[order(gv, v)]
  offset <- cumsum(c(0, n_ok))[seq_len(n_groups)]
  q <- lapply(quantiles, function(p) {
    idx <- 1 + pmax(n_ok - 1, 0) * p
    lo <- floor(idx)
    hi <- ceiling(idx)
    x_lo <- vs[offset + lo]
    x_hi <- vs[offset + hi]
    h <- idx - lo
    res <- ifelse(h > 0 & x_hi != x_lo, (1 - h) * x_lo + h * x_hi, x_lo)
    res[n_ok == 0] <- NA_real_
    res
  })
  names(q) <- q_names

  dplyr::bind_cols(
    keys,
    tibble::tibble(mean = mean_v, sd = sd_v,
                   n_ens = tabulate(g, nbins = n_groups)),
    tibble::as_tibble(q))
}

#' Interpolate depth-resolved ensemble output to fixed depths
#'
#' For every member, date and depth-resolved variable, linearly interpolates the
#' profile (stored depths are metres below the surface) to each requested depth.
#' Variables without a depth dimension are returned unchanged.
#'
#' With `depth_ref = "surface"` a depth `d` is `d` m below the surface. With
#' `"bottom"` it is `d` m above the lake bed, which on a given date and member is
#' `level - d` m below the surface, where `level` is that member's `LKE_lvlwtr`
#' on that date. A request outside the water column (below the bed, or above the
#' surface) gives no value for that member and date; between the outermost grid
#' depths and the surface or bed the nearest grid value is used, as
#' `interp_static_grid()` does. A profile needs two or more values.
#'
#' The interpolation is vectorised across profiles: profiles are sorted by depth
#' within id and a single `findInterval()` on a composite `id * scale + depth`
#' key finds each bracket, so there is no loop over the (many) profiles.
#'
#' @param ens_df long data.frame with `date`, `var_sim`, `depth`, `value` and
#'   `ens`; as built inside [write_aeme_parquet()].
#' @param depths numeric; the depths to interpolate to.
#' @param depth_ref `"surface"` or `"bottom"`.
#' @return A data.frame with the columns of `ens_df`. Depth-resolved rows are
#'   at the requested `depths`; rows with no value are dropped.
#' @noRd
interpolate_to_depths <- function(ens_df, depths, depth_ref = "surface") {

  scalar <- ens_df[is.na(ens_df$depth), , drop = FALSE]
  prof <- ens_df[!is.na(ens_df$depth) & !is.na(ens_df$value), , drop = FALSE]
  if (nrow(prof) == 0) return(scalar[!is.na(scalar$value), , drop = FALSE])

  grp <- dplyr::group_by(prof, dplyr::across(dplyr::all_of(
    c("ens", "date", "var_sim"))))
  keys <- dplyr::group_keys(grp)
  id <- dplyr::group_indices(grp)
  n_prof <- nrow(keys)

  # water level of each member on each date, for the water-column limits and for
  # bottom-referenced depths
  lvl <- scalar[scalar$var_sim == "LKE_lvlwtr" & !is.na(scalar$value), ,
                drop = FALSE]
  level <- lvl$value[match(paste(keys$ens, keys$date),
                           paste(lvl$ens, lvl$date))]
  if (depth_ref == "bottom" && all(is.na(level))) {
    cli::cli_abort(c(
      "{.code depth_ref = \"bottom\"} needs the water level.",
      "i" = "{.field LKE_lvlwtr} is not among the model's output variables."))
  }

  o <- order(id, prof$depth)
  x <- prof$depth[o]
  y <- prof$value[o]
  pid <- id[o]
  cnt <- tabulate(pid, nbins = n_prof)
  first <- cumsum(c(0, cnt))[seq_len(n_prof)] + 1   # index of each profile's top
  last <- first + cnt - 1                           # ... and of its bottom
  scale <- 10^ceiling(log10(max(x, depths, level, 1, na.rm = TRUE) + 1) + 1)
  xk <- pid * scale + x                             # ascending: sorted by id, x

  one_depth <- function(d) {
    tgt <- if (depth_ref == "surface") rep(d, n_prof) else level - d
    inside <- !is.na(tgt) & tgt >= 0 & (is.na(level) | tgt <= level) & cnt >= 2
    tk <- seq_len(n_prof) * scale + pmin(pmax(tgt, 0), scale / 2)
    tk[is.na(tk)] <- 0
    lo <- findInterval(tk, xk)
    below <- lo < first                  # shallower than the top grid depth
    above <- lo >= last                  # deeper than the bottom grid depth
    lo[below] <- first[below]
    hi <- lo + 1L
    hi[below] <- lo[below]
    hi[above] <- last[above]
    lo[above] <- last[above]
    w <- (tgt - x[lo]) / (x[hi] - x[lo])
    w[!is.finite(w)] <- 0
    w <- pmin(pmax(w, 0), 1)
    val <- round(y[lo] + w * (y[hi] - y[lo]), 4)
    val[!inside] <- NA_real_
    data.frame(date = keys$date, var_sim = keys$var_sim, depth = d,
               value = val, ens = keys$ens, stringsAsFactors = FALSE)
  }

  out <- dplyr::bind_rows(c(list(scalar), lapply(depths, one_depth)))
  out[!is.na(out$value), , drop = FALSE]
}
