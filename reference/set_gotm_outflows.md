# Set outflow data for a GOTM-WET simulation directory

Thin, `aeme`-free wrapper around the internal outflow writer used by
[`build_aeme()`](https://limnotrack.com/reference/build_aeme.md). Writes
`inputs/outf_<name>.dat` per outflow into `path_gotm`, and updates the
`streams` block of `gotm.yaml` to point at them. Existing stream entries
not named in `outf` are left untouched.

## Usage

``` r
set_gotm_outflows(
  path_gotm,
  outf,
  outf_factor = 1,
  yaml_file = file.path(path_gotm, "gotm.yaml")
)
```

## Arguments

- path_gotm:

  filepath; to GOTM-WET directory (containing `gotm.yaml` and an
  `inputs/` subdirectory)

- outf:

  named list of data.frames, one per outflow, each with a `Date` column
  and a `HYD_flow` column – see
  [`add_outflows()`](https://limnotrack.com/reference/add_outflows.md)
  for the expected schema.

- outf_factor:

  numeric; scaling factor applied to all outflow flow rates. Default is
  `1`.

- yaml_file:

  filepath; path to the `gotm.yaml` file to update. Defaults to
  `gotm.yaml` in `path_gotm`.

## Value

invisibly, the updated yaml list

## Examples

``` r
if (FALSE) { # \dontrun{
set_gotm_outflows(path_gotm, outf = list(outlet_1 = outflow_df))
} # }
```
