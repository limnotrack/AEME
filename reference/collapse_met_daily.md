# Collapse a sub-daily meteo data frame to a daily time step

Aggregates a `POSIXct`-stamped (or otherwise sub-daily) meteorological
data frame to one row per calendar day: every numeric column is averaged
over the day, except `MET_pprain` / `MET_ppsnow`, whose aggregation is
controlled by `precip`. Non-numeric columns other than `Date` are
dropped, and `Date` is returned as a `Date`.

## Usage

``` r
collapse_met_daily(obs_met, precip = c("mean", "sum"))
```

## Arguments

- obs_met:

  data frame; meteo forcing with a `Date` column (`Date` or `POSIXct`)
  and numeric `MET_*` columns.

- precip:

  character; how to aggregate `MET_pprain` / `MET_ppsnow` over the day.
  `"mean"` (default) when they are already a mm/day rate; `"sum"` when
  they are per-time-step accumulations.

## Value

A data frame with one row per day: `Date` (as `Date`) and the
day-aggregated numeric columns.

## Details

AEME defines `MET_pprain` / `MET_ppsnow` as a **rate in mm/day** (see
[`standardise_met()`](https://limnotrack.com/reference/standardise_met.md)).
If the frame is already on that convention – e.g. the output of
[`standardise_met()`](https://limnotrack.com/reference/standardise_met.md),
which is what
[`build_aeme()`](https://limnotrack.com/reference/build_aeme.md) feeds
the water balance – a daily *mean* of the rate is the daily rate, so
`precip = "mean"` (the default) is correct. Raw sub-daily reanalysis /
AWS products instead report precipitation as an **accumulation per time
step** (mm that fell during that step); for those, use `precip = "sum"`
to get the daily total.

## See also

[`standardise_met()`](https://limnotrack.com/reference/standardise_met.md)

## Examples

``` r
hourly <- data.frame(
  Date = seq(as.POSIXct("2020-01-01", tz = "UTC"), by = "hour",
             length.out = 48),
  MET_tmpair = rnorm(48, 15, 3),
  MET_pprain = c(rep(0, 20), rep(0.5, 4), rep(0, 24))  # mm per hour
)
collapse_met_daily(hourly, precip = "sum")
#> # A tibble: 2 × 3
#>   Date       MET_tmpair MET_pprain
#>   <date>          <dbl>      <dbl>
#> 1 2020-01-01       15.7          2
#> 2 2020-01-02       15.2          0
```
