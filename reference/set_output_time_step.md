# Set the model output temporal frequency

A thin, generic wrapper around
[`set_time()`](https://limnotrack.com/reference/set_time.md) for the
common case of only wanting to change how often every model in the
ensemble writes output. The frequency applies to all models:
`build_glm()`, `build_gotm()` and `build_simstrat()` each translate
`output_time_step` into their native output-cadence setting (`nsave`,
`output.yaml` `time_step`, and `Output/Times` respectively), and the
internal `aeme_time_axis()` reconstructs the matching output time axis
when the results are read back.

## Usage

``` r
set_output_time_step(aeme, frequency, daily_mean = FALSE)
```

## Arguments

- aeme:

  An [Aeme](https://limnotrack.com/reference/Aeme.md) object.

- frequency:

  The desired output frequency. Either a single positive number of
  **seconds**, or a character string:

  - a named frequency: `"subdaily"`/`"hourly"` (3600 s), `"daily"`
    (86400 s), `"weekly"` (604800 s);

  - a `"<n> <unit>"` string, e.g. `"6 hours"`, `"30 min"`, `"1 day"`,
    `"900 sec"` (units: `sec`/`min`/`hour`/`day`/`week`, singular or
    plural).

- daily_mean:

  logical; if `TRUE`, also produce a daily-mean output stream (see
  Details). Default `FALSE`.

## Value

The `aeme` object with `time(aeme)$output_time_step` (and, when
supplied, `time(aeme)$output_daily_mean`) updated. `output_time_step`
must be greater than or equal to `time(aeme)$time_step`;
[`set_time()`](https://limnotrack.com/reference/set_time.md) raises
`aeme_error_output_time_step` otherwise.

## Details

AEME does not temporally disaggregate forcing, so requesting output more
frequent than daily is only meaningful when the meteorological (and,
where relevant, inflow/outflow) forcing is supplied at least as often.
See `vignette("hourly-vs-daily-met")`.

### Daily-mean output

`daily_mean = TRUE` makes every model additionally produce a
**daily-mean** output stream alongside the raw `frequency` output, so a
run can be done at a sub-daily cadence (for accuracy, or to average out
a diurnal cycle) while the results compared against daily observations
are true daily means rather than instantaneous snapshots. It is modelled
on GOTM's native `output_daily` stream (`time_method: mean`, one record
per day):

- **GOTM-WET** writes the daily means itself (its `output_daily.nc`).

- **GLM-AED** and **Simstrat** have no native time-averaging, so AEME
  averages their sub-daily `output.nc` by calendar day after the run and
  writes a companion `output_daily.nc`. The raw sub-daily output is
  kept.

The daily stream stores only the targeted variables — the
`model_controls` `simulate` set plus the fixed set AEME's readers
require — not every model variable.

## See also

[`set_time()`](https://limnotrack.com/reference/set_time.md)

## Examples

``` r
aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
aeme <- set_output_time_step(aeme, "hourly")
time(aeme)$output_time_step
#> [1] 3600
aeme <- set_output_time_step(aeme, "6 hours")
time(aeme)$output_time_step
#> [1] 21600
aeme <- set_output_time_step(aeme, "hourly", daily_mean = TRUE)
time(aeme)$output_daily_mean
#> [1] TRUE
```
