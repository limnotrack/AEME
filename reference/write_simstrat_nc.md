# Consolidate Simstrat-AED2 text output into a single netCDF file

Simstrat writes one plain-text `.dat` file per output variable (see
`strat_outputfile.f90` in the Simstrat source), unlike GLM-AED and
GOTM-WET which write netCDF directly. This function reads every
`<var>_out.dat` file in the simulation's output directory and writes
them into a single compressed `output.nc`, so that Simstrat-AED2 output
can be read with the same netCDF-based tooling
([`read_model_outputs`](https://limnotrack.com/reference/read_model_outputs.md),
[`get_model_outfile`](https://limnotrack.com/reference/get_model_outfile.md),
...) used for the other models, and so the on-disk output is much
smaller than the raw text files.

## Usage

``` r
write_simstrat_nc(sim_folder, remove_dat = TRUE)
```

## Arguments

- sim_folder:

  character; path to the `simstrat_aed2` simulation directory
  (containing `simstrat.par` and the output directory referenced by its
  `Output.Path`).

- remove_dat:

  logical; delete the source `<var>_out.dat` files after they have been
  written to `output.nc`. Default `TRUE` (this is the actual disk-space
  saving – keeping both would use more space, not less).

## Value

Invisibly returns the path to the written `output.nc` file, or `NULL` if
no output files were found.
