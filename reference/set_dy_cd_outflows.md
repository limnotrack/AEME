# Set outflow data for a DYRESM-CAEDYM simulation directory

Thin, `aeme`-free wrapper around the internal outflow writer used by
[`build_aeme()`](https://limnotrack.com/reference/build_aeme.md). Writes
`<lakename>.wdr` into `path_dy` and, so the outlet set stays consistent,
rebuilds the outlet block of `<lakename>.stg` with the supplied heights.

## Usage

``` r
set_dy_cd_outflows(
  path_dy,
  outf,
  heights_wdr,
  outf_factor = 1,
  update_stg = TRUE
)
```

## Arguments

- path_dy:

  filepath; the `dy_cd` model directory (containing the `<lakename>.stg`
  file).

- outf:

  named list of data.frames, one per outflow, each with a `Date` column
  and a `HYD_flow` column – see
  [`add_outflows()`](https://limnotrack.com/reference/add_outflows.md)
  for the expected schema.

- heights_wdr:

  named numeric vector; outlet elevation (m ASL) for each name in
  `outf`.

- outf_factor:

  numeric; scaling factor applied to all outflow flow rates. Default is
  `1`.

- update_stg:

  logical; also rebuild the outlet block of `<lakename>.stg` with
  `heights_wdr`. Default `TRUE`.

## Value

invisibly, `NULL`.

## Details

DYRESM-CAEDYM's `.wdr` writer (`make_dy_wdr()`) only supports a single
outflow series (or the internal water-balance `outflow`/`wbal` pair), so
`outf` should normally hold exactly one data.frame.

## Examples

``` r
if (FALSE) { # \dontrun{
set_dy_cd_outflows(path_dy, outf = list(outflow = outflow_df),
                   heights_wdr = c(outflow = 12.07))
} # }
```
