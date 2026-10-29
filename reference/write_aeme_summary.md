# Write and read an AemeSummary

`write_aeme_summary()` saves a summary for use elsewhere, for example in
a dashboard. `read_aeme_summary()` reads one written as `"rds"` or
`"parquet"`.

## Usage

``` r
write_aeme_summary(x, path, format = c("rds", "parquet", "json"))

read_aeme_summary(path)
```

## Arguments

- x:

  An
  [AemeSummary](https://limnotrack.com/AEME/reference/AemeSummary.md).

- path:

  file path (`"rds"`, `"json"`) or directory (`"parquet"`).

- format:

  `"rds"`, `"parquet"` or `"json"`.

## Value

`write_aeme_summary()` returns `path` invisibly; `read_aeme_summary()`
an `AemeSummary`.

## Details

- `"rds"`: one `.rds` file holding the whole object (the default).

- `"parquet"`: a directory with one parquet file per table (`output`,
  `profiles`, `hypsograph`, `parameters`, `observations_*`, `forcing_*`)
  and a `meta.rds` with the rest. Needs the `arrow` package.

- `"json"`: one `.json` file with every table and the metadata, for
  dashboards that are not written in R. It cannot be read back.
