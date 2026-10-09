# Set time parameters for an Aeme object

Set time parameters for an Aeme object

## Usage

``` r
set_time(
  aeme,
  start,
  stop,
  spin_up,
  time_step,
  output_time_step,
  output_daily_mean,
  tz
)
```

## Arguments

- aeme:

  Aeme object.

- start, stop:

  Time in the format "YYYY-mm-dd" or "YYYY-mm-dd HH:MM" or "YYYY-mm-dd
  HH:MM:SS". Interpreted as wall-clock time in the object's timezone
  (`time(aeme)$tz`, or `tz` if supplied here) and stored as UTC.

- spin_up:

  Spin-up time in days. Can be a single numeric value or a list with
  model names as names and numeric values as values.

- time_step:

  numeric; model integration time step in seconds. Default (when unset
  on the object) 3600.

- output_time_step:

  numeric; model output time step in seconds. Must be greater than or
  equal to `time_step`. Default (when unset on the object) 86400
  (daily). Set to e.g. 3600 for hourly output. Note that AEME does not
  temporally disaggregate forcing: sub-daily output requires forcing
  supplied at (at least) the same cadence.

- output_daily_mean:

  logical; if `TRUE`, every model additionally produces a daily-mean
  output stream alongside its raw `output_time_step` output. Default
  (when unset on the object) `FALSE`. See
  [`set_output_time_step`](https://limnotrack.com/reference/set_output_time_step.md).

- tz:

  character; Olson timezone in which user-supplied timestamps (here and
  in the forcing/observation inputs) are expressed. Stored on the object
  as `time$tz` and used to convert those timestamps to UTC internally
  and for display. If omitted, the object's existing `time$tz` is kept.

## Value

Aeme object with time parameters set

## Examples

``` r
aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
aeme <- readRDS(aeme_file)
aeme <- set_time(aeme = aeme, start = "2020-01-01", stop = "2020-12-31",
                 spin_up = 35)
```
