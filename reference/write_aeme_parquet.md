# Export an Aeme object's model output to a partitioned Parquet store

Writes the long-format model output already loaded onto `aeme` (see
[`load_output`](https://limnotrack.com/reference/load_output.md)) into a
Hive-partitioned Parquet store suitable for querying across many lakes
at once (e.g. with DuckDB), plus updates a single `lakes.parquet`
dimension table describing every lake exported into `out_root` so far.

## Usage

``` r
write_aeme_parquet(
  aeme,
  out_root,
  model,
  quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95),
  compression = "gzip"
)
```

## Arguments

- aeme:

  Aeme object with output loaded (see
  [`load_output`](https://limnotrack.com/reference/load_output.md)).

- out_root:

  character; root directory of the store (created if missing).

- model:

  character vector of model codes to export (see
  [`list_models`](https://limnotrack.com/reference/list_models.md)).
  Default: every model with output on `aeme`.

- quantiles:

  numeric vector of quantiles (in `[0, 1]`) for the ensemble summary
  tier. Default `c(0.05, 0.25, 0.5, 0.75, 0.95)`.

- compression:

  character; Parquet compression codec, passed to
  [`arrow::write_parquet()`](https://arrow.apache.org/docs/r/reference/write_parquet.html).
  Default `"gzip"` – slower to write than `arrow`'s own default
  (`"snappy"`) but meaningfully smaller on disk for this kind of
  repetitive numeric time series, which matters more here than write
  speed since a store is written occasionally and read often. Use
  `"snappy"` instead if write/decode speed matters more than size for
  your case, or `"zstd"` for a middle ground.

## Value

(Invisibly) a list with `lake_row` (the dimension-table row just
written), `raw_files`, and `summary_files` (paths written; empty if the
summary tier was skipped because `aeme` only has one ensemble member).

## Details

Two output tiers are written per model, partitioned as
`<tier>/lake_id=<id>/model=<model>/part-0.parquet`:

- `output_raw`: every ensemble member's full time series (`date`, `ens`,
  `var_sim`, `depth`, `value`) – for drill-down into a single lake (e.g.
  a spaghetti plot across ensemble members).

- `output_summary`: `value` collapsed across the ensemble dimension at
  each `date`/`var_sim`/`depth` into `mean`, `sd`, `n_ens`, and the
  requested quantiles (named `q05`, `q50`, ... from `quantiles`) – for
  overview views that scan many lakes at once, so the ensemble collapse
  doesn't need recomputing on every load.

`output_summary` is only written when `aeme` has more than one ensemble
member: with a single member, collapsing across `ens` is a no-op that
would just re-store the same values under 8 columns (`mean`, `sd`,
`n_ens`, 5 quantile columns) instead of 1 (`value`) – pure overhead, no
new information. Readers should check a lake's `n_ens` in
`lakes.parquet` (see
[`read_aeme_parquet`](https://limnotrack.com/reference/read_aeme_parquet.md))
and fall back to `output_raw` for any lake where it's `1`.

Re-running this for one lake only rewrites that lake's own partition
directories and its one row in `lakes.parquet`; other lakes already
exported into `out_root` are untouched. If a lake's ensemble count
changes between exports (e.g. it grows from 1 to many members), any
stale `output_summary` partition for that lake/model is removed or
(re)written to match.

## Examples

``` r
if (FALSE) { # \dontrun{
aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
aeme <- readRDS(aeme_file)
write_aeme_parquet(aeme, out_root = file.path(tempdir(), "parquet"))
} # }
```
