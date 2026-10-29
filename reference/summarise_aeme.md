# Summarise an Aeme object

Reduces an [Aeme](https://limnotrack.com/AEME/reference/Aeme.md) with
model output to an
[AemeSummary](https://limnotrack.com/AEME/reference/AemeSummary.md):
daily surface and bottom values for every depth-resolved variable (the
split is at the epilimnion depth, `HYD_epidep`, else the thermocline
depth, `HYD_thmcln`, else half the water column), daily values for the
rest, and, optionally, the observations, daily forcing and the
hypsograph. The result is small enough to store and use in a dashboard,
and has no model configuration, so it cannot be used to build or run a
model.

## Usage

``` r
summarise_aeme(
  aeme,
  model,
  ens_n = NULL,
  stats = c("mean", "min", "max", "q05", "q95"),
  keep_members = FALSE,
  vars = NULL,
  observations = TRUE,
  forcing = c("meteo", "inflows", "outflows", "water_balance"),
  profiles = FALSE,
  digits = 4L
)
```

## Arguments

- aeme:

  Aeme object.

- model:

  character; models to summarise. Default: those with output.

- ens_n:

  integer; ensemble members to use. Default `NULL`: all.

- stats:

  character; statistics across members when there is more than one:
  `"mean"`, `"min"`, `"max"`, `"sd"`, or quantiles such as `"q05"` and
  `"q95"`. Ignored for a single member (the stat is then `"value"`).

- keep_members:

  logical; also keep each member's series (as stats named `"ens_001"`,
  ...). Default `FALSE`.

- vars:

  character; AEME variables to keep. Default `NULL`: every standard
  variable in the output.

- observations:

  logical; keep the observations. Default `TRUE`.

- forcing:

  character; which forcing tables to keep, from `"meteo"`, `"inflows"`,
  `"outflows"` and `"water_balance"` (all by default). Use
  `character(0)` for none. They are daily means.

- profiles:

  logical; also calculate seasonal mean profiles of temperature and
  oxygen (see
  [calc_seasonal_profiles](https://limnotrack.com/AEME/reference/calc_seasonal_profiles.md)-style
  output in the `profiles` slot). Default `FALSE`.

- digits:

  integer; values are rounded to this many decimal places.

## Value

An [AemeSummary](https://limnotrack.com/AEME/reference/AemeSummary.md)
object.

## Details

With more than one ensemble member the member values are combined into
the statistics named in `stats`.

## See also

[`write_aeme_summary()`](https://limnotrack.com/AEME/reference/write_aeme_summary.md),
[`read_aeme_summary()`](https://limnotrack.com/AEME/reference/write_aeme_summary.md)
