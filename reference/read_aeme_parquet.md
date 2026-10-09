# Read from a Parquet store written by write_aeme_parquet()

Queries the Hive-partitioned Parquet store produced by
[`write_aeme_parquet`](https://limnotrack.com/reference/write_aeme_parquet.md),
applying any of `lake_id`, `model`, `var_sim`, and `date_range` as
filters *before* the data is read into memory – partition pruning
(`lake_id`/`model`) skips whole files, and the `var_sim`/`date_range`
filters are pushed down into the Parquet scan by `arrow`, so filtering
to e.g. one variable for one lake never touches the other
lakes/variables on disk.

## Usage

``` r
read_aeme_parquet(
  out_root,
  what = c("output_summary", "output_raw", "lakes"),
  lake_id = NULL,
  model = NULL,
  var_sim = NULL,
  date_range = NULL,
  collect = TRUE
)
```

## Arguments

- out_root:

  character; root directory of the parquet store (as passed to
  [`write_aeme_parquet`](https://limnotrack.com/reference/write_aeme_parquet.md)).

- what:

  character; which part of the store to read: `"output_summary"`
  (default; ensemble-collapsed `mean`/`sd`/quantiles per
  `date`/`var_sim`/`depth`), `"output_raw"` (every ensemble member), or
  `"lakes"` (the one-row-per-lake dimension table).

- lake_id:

  character vector; restrict to these lake ids. Default: all lakes in
  the store. Applied to `what = "lakes"` too.

- model:

  character vector; restrict to these model codes (see
  [`list_models`](https://limnotrack.com/reference/list_models.md)).
  Ignored for `what = "lakes"`.

- var_sim:

  character vector; restrict to these `var_sim` codes. Ignored for
  `what = "lakes"`.

- date_range:

  length-2 Date/POSIXct/character vector; restrict to
  `date >= date_range[1] & date <= date_range[2]`. Ignored for
  `what = "lakes"`.

- collect:

  logical; if `TRUE` (default), materialise the result as a tibble. If
  `FALSE`, return the lazy `arrow` query so further `dplyr` verbs (e.g.
  an additional `group_by()`/`summarise()`) can be chained – and pushed
  down where possible – before
  [`dplyr::collect()`](https://dplyr.tidyverse.org/reference/compute.html).

## Value

A tibble, or a lazy `arrow` `Dataset`/query object if `collect = FALSE`.

## Details

`what = "output_summary"` only has data for lakes with more than one
ensemble member (see
[`write_aeme_parquet`](https://limnotrack.com/reference/write_aeme_parquet.md));
requesting it for a single-ensemble lake returns 0 rows for that lake,
with a warning pointing at `output_raw` instead.

## Examples

``` r
if (FALSE) { # \dontrun{
out_root <- file.path(tempdir(), "dashboard")

# every lake's ensemble-summary surface temperature
surf_temp <- read_aeme_parquet(out_root, var_sim = "HYD_temp") |>
  dplyr::filter(depth == min(depth))

# full ensemble spread for one lake, for a spaghetti plot
ens <- read_aeme_parquet(out_root, what = "output_raw",
                           lake_id = "LID45819", var_sim = "HYD_temp")

lakes <- read_aeme_parquet(out_root, what = "lakes")
} # }
```
