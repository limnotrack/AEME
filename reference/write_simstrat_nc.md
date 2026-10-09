# Consolidate Simstrat-AED2/AED text output into a single netCDF file

Simstrat writes one plain-text `.dat` file per output variable (see
`strat_outputfile.f90` in the Simstrat source), unlike GLM-AED and
GOTM-WET which write netCDF directly. This function reads every
`<var>_out.dat` file in the simulation's output directory and writes
them into a single compressed `output.nc`, so that Simstrat output can
be read with the same netCDF-based tooling
([`read_model_outputs`](https://limnotrack.com/reference/read_model_outputs.md),
[`get_model_outfile`](https://limnotrack.com/reference/get_model_outfile.md),
...) used for the other models, and so the on-disk output is much
smaller than the raw text files.

## Usage

``` r
write_simstrat_nc(sim_folder, config_file = "simstrat.par", remove_dat = FALSE)
```

## Arguments

- sim_folder:

  character; path to the `simstrat_aed2`/`simstrat_aed` simulation
  directory (containing `simstrat.par` and the output directory
  referenced by its `Output.Path`).

- remove_dat:

  logical; delete the source `<var>_out.dat` files after they have been
  written to `output.nc`. Default `FALSE` – deleting them is the actual
  disk-space saving (keeping both uses more space, not less), but it
  also removes what
  [`read_simstrat_dat`](https://limnotrack.com/reference/read_simstrat_dat.md)
  reads.

  Each netCDF variable's `units` and `long_name` attributes are looked
  up from the package's `key_naming` table (matched on the variable's
  native Simstrat-AED2/AED name, sediment-zone variables under their
  base name with the `_zone` suffix stripped). A variable not present in
  `key_naming` is written with empty `units` and `long_name` equal to
  its netCDF variable name.

## Value

Invisibly returns the path to the written `output.nc` file, or `NULL` if
no output files were found.

## Details

AED's sediment-zone output (Simstrat-AED only, not AED2) is written as a
second, separate family of files, `<var>_zone_out.dat` – one column per
benthic zone (labelled by that zone's reference depth), alongside the
regular `<var>_out.dat` for the same variable (a single lake-bottom or
whole-lake value). Because both exist side by side for the same variable
name and `_zone_out.dat` itself ends in `_out.dat`, these can't share
the water-column `z` dimension/grid the way regular depth-profile
variables do – doing so silently wrote zone values against the wrong
depths (or failed outright on a column-count mismatch) whenever a zone
file's column count didn't happen to match the shared `z` grid's length.
They get their own `zone` netCDF dimension instead, coordinate-valued by
each zone's reference depth (m). Variable names keep their `_zone`
suffix (stripping only the trailing `_out.dat`, as for every other
file), so e.g. `NIT_amm_dsf_out.dat` and `NIT_amm_dsf_zone_out.dat`
become the distinct netCDF variables `NIT_amm_dsf` (dims `time`) and
`NIT_amm_dsf_zone` (dims `zone, time`) – never colliding. Reading them
back needs no extra code:
[`read_simstrat_output`](https://limnotrack.com/reference/read_simstrat_output.md)'s
`load_all` sweep already routes any variable whose dimensions aren't
`(time)` or `(z, time)` through `.read_glm_grouped_var` into an
[`new_grouped_var`](https://limnotrack.com/reference/new_grouped_var.md)
object, the same generic path GLM-AED's own `nzones`-dimensioned output
uses.
