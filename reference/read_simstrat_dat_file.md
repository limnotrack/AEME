# Read a single Simstrat `<var>_out.dat` output file

Simstrat writes one plain-text file per output variable (see
`strat_outputfile.f90::open_files()` in the Simstrat source): a header
row of `Datetime,<z1>,<z2>,...` – depths (or, for `_zone_out.dat` files,
zone heights) written with Fortran `F12.3`, a single trailing column for
surface/whole-lake variables – followed by one `(F12.4)` day number plus
`(ES14.4E3)` values per output time.

## Usage

``` r
read_simstrat_dat_file(file, skip_rows = 0L, n_rows = -1L)
```

## Arguments

- file:

  character; path to a `<var>_out.dat` file.

- skip_rows:

  integer; number of *data* rows (after the header) to skip. Lets a
  caller read only the part of a file it needs, rather than parsing rows
  it will immediately discard. Default `0`.

- n_rows:

  integer; number of data rows to read after `skip_rows`, or `-1`
  (default) for all remaining rows.

## Value

List with elements

- `day`:

  numeric vector of Simstrat day numbers, one per row read.

- `depths`:

  numeric vector of the header's depths/zone heights, one per value
  column (length 1 for a surface/whole-lake variable). As written by
  Simstrat: negative-down offsets from the lake surface for water-column
  variables, positive heights above the lake bottom for `_zone`
  variables.

- `values`:

  numeric matrix, `length(day)` rows x `length(depths)` columns, in the
  file's own column order.

- `offset`:

  integer; `skip_rows`, so a caller can map row indices in the full file
  onto rows of `values`.

## Details

Everything in the file is numeric, so it is read with a single
[`scan`](https://rdrr.io/r/base/scan.html) call rather than
[`read.csv`](https://rdrr.io/r/utils/read.table.html) – substantially
faster on the large depth x time files, which matters when a calibration
reads the output once per model run (see
[`read_simstrat_dat`](https://limnotrack.com/reference/read_simstrat_dat.md)).
