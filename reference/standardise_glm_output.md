# Standardise a raw GLM-AED output list onto AEME's common depth grid

Interpolates every `(z, time)` variable in a
`read_glm_output(raw_output = TRUE)` list from GLM's own, time-varying
layer structure – captured in its `z` entry (see
[`read_glm_output()`](https://limnotrack.com/reference/read_glm_output.md))
– onto a shared depth grid, the same one `raw_output = FALSE` produces,
without re-reading the netCDF file. Also renames variables back to their
AEME `var_aeme` names (where
[key_naming](https://limnotrack.com/reference/key_naming.md) has a
translation) and re-applies AED unit-conversion factors, so the result
resembles `read_glm_output(raw_output = FALSE)`'s output as closely as
possible.

## Usage

``` r
standardise_glm_output(out_raw, depths = NULL)
```

## Arguments

- out_raw:

  list; an `aeme_output_raw`-classed GLM-AED output list from
  [`read_glm_output()`](https://limnotrack.com/reference/read_glm_output.md)
  with `raw_output = TRUE` (must include its `z` and `LKE_lvlwtr`
  entries).

- depths:

  numeric vector; depths to interpolate onto. If `NULL` (default), uses
  the same standardised depth-fraction grid
  `read_glm_output(raw_output = FALSE)` uses (see
  [model_layer_structure](https://limnotrack.com/reference/model_layer_structure.md)).

## Value

An `aeme_output`-classed list (see
[`is_aeme_output()`](https://limnotrack.com/reference/is_aeme_output.md)),
structurally the same as `read_glm_output(raw_output = FALSE)`'s return
value.

## Details

Useful when you've already loaded raw output (e.g. to inspect native
units/names) and only later decide you want it on AEME's standard grid
too – interpolating in place is cheaper than re-opening the netCDF file
and reading it again with `raw_output = FALSE`.

The `diag` and `sediment` sub-lists (variables with dimensions other
than `(time)`/`(z, time)`, e.g. sediment-zone variables, plus scalars)
are carried over unchanged – they aren't on a depth grid to begin with,
so there's nothing to interpolate.

## Examples

``` r
if (FALSE) { # \dontrun{
out_raw <- read_glm_output(file = outfile, raw_output = TRUE)
out_std <- standardise_glm_output(out_raw)
plot_model_output(out_std, "HYD_temp")
} # }
```
