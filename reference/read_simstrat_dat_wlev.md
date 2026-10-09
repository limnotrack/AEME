# Read Simstrat lake water level from the raw text output

The
[`read_simstrat_wlev`](https://limnotrack.com/reference/read_simstrat_wlev.md)
equivalent for Simstrat's own `WaterH_out.dat`, for callers that have
not written (or have deleted) the consolidated `output.nc`.

## Usage

``` r
read_simstrat_dat_wlev(
  sim_folder = NULL,
  config_file = "simstrat.par",
  out_dir = NULL,
  ref_year = NULL
)
```

## Arguments

- sim_folder:

  character; path to the `simstrat_aed2`/`simstrat_aed` simulation
  directory (containing `simstrat.par` and the output directory it
  points at). Not needed if both `out_dir` and `ref_year` are supplied.

- config_file:

  character; name of (or path to) the Simstrat configuration file.
  Default `"simstrat.par"`.

- out_dir:

  character; the output directory itself, skipping the `simstrat.par`
  lookup. Default `NULL` (read from `simstrat.par`).

- ref_year:

  integer; the simulation's `Simulation.Reference year`, skipping the
  `simstrat.par` lookup. Default `NULL` (read from `simstrat.par`).
  Supply both this and `out_dir` in a tight loop to avoid re-parsing the
  configuration on every read.

## Value

Data frame with Date and LKE_lvlwtr columns.
