# Check Simstrat par file for common issues

Check Simstrat par file for common issues

## Usage

``` r
check_simstrat_par(file, output_time_step = 86400)
```

## Arguments

- file:

  path to Simstrat `.par` (JSON) file

- output_time_step:

  numeric; expected model output step in seconds
  (`time$output_time_step`). `Output.Times * Simulation.Timestep s` must
  equal this. Default 86400 (one output row per day).

## Value

Invisibly returns TRUE if no issues found, otherwise aborts with
informative messages
