# Create a new, minimal Aeme object

Returns a valid, fully-populated `Aeme` object built from placeholder
values, intended as a starting point for a new lake configuration.
Unlike
[`aeme_constructor()`](https://limnotrack.com/reference/aeme_constructor.md),
which requires real lake, time, and input data and aborts without it,
`new_aeme()` fills in sensible defaults so you get a live object back
immediately, ready to be built up incrementally with the slot setters
(e.g. `lake<-`, `time<-`, `input<-`) and helpers such as
[`add_hypsograph()`](https://limnotrack.com/reference/add_hypsograph.md),
[`add_met()`](https://limnotrack.com/reference/add_met.md), and
[`add_inflows()`](https://limnotrack.com/reference/add_inflows.md).

## Usage

``` r
new_aeme(
  name = "newlake",
  id = "0001",
  latitude = 0,
  longitude = 0,
  elevation = 100,
  depth = 10,
  area = 1e+05,
  start = Sys.Date() - 365,
  stop = Sys.Date(),
  time_step = 3600,
  output_time_step = 86400,
  output_daily_mean = FALSE,
  tz = "UTC",
  Kw = 1
)
```

## Arguments

- name:

  character; lake name (alphanumeric only). Default `"newlake"`.

- id:

  character; lake identifier (alphanumeric only). Default `"0001"`.

- latitude:

  numeric; lake latitude, in \\\[-90, 90\]\\. Default `0`.

- longitude:

  numeric; lake longitude, in \\\[-180, 180\]\\. Default `0`.

- elevation:

  numeric; lake surface elevation above sea level (m). Default `100`.

- depth:

  numeric; lake depth (m). Default `10`.

- area:

  numeric; lake surface area (m^2). Default `1e5`.

- start:

  character, Date, or POSIXct; simulation start date. Default one year
  before `stop`.

- stop:

  character, Date, or POSIXct; simulation stop date. Default today.

- time_step:

  numeric; model integration time step in seconds. Default `3600` (1
  hour).

- output_time_step:

  numeric; model output time step in seconds. Must be `>= time_step`.
  Default `86400` (daily).

- output_daily_mean:

  logical; if `TRUE`, every model additionally produces a daily-mean
  output stream (see
  [`set_output_time_step()`](https://limnotrack.com/reference/set_output_time_step.md)).
  Default `FALSE`.

- tz:

  character; Olson timezone in which `start`, `stop` and any forcing
  timestamps are expressed. Stored as `time$tz`; timestamps are
  converted to UTC internally. Default `"UTC"` – set a non-UTC zone only
  when your source data is in local time.

- Kw:

  numeric; light extinction coefficient (m^-1). Default `1`.

## Value

A valid `Aeme` object populated with placeholder values.

## See also

[`aeme_constructor()`](https://limnotrack.com/reference/aeme_constructor.md)
for building an `Aeme` object from real lake data with full validation.

## Examples

``` r
aeme <- new_aeme()

aeme <- new_aeme(name = "mylake", id = "001", latitude = -37.8,
                 longitude = 175.3, elevation = 30, depth = 15,
                 area = 2.5e5)
```
