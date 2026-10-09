# Standardise meteorological variable names and units for AEME

Attempts to match column names in a meteorological data frame to AEME
standard variable names using
[`guess_aeme_vars()`](https://limnotrack.com/reference/guess_aeme_vars.md),
then detects the likely input units of each variable from its values and
converts to the units expected by the package.

## Usage

``` r
standardise_met(
  met,
  verbose = TRUE,
  precip_accum = TRUE,
  tz = "UTC",
  longitude = NULL
)
```

## Arguments

- met:

  data.frame; meteorological data with a `Date` column and one or more
  meteorological variable columns.

- verbose:

  logical; if `TRUE` (default), emit `cli_inform` messages describing
  each detected unit conversion applied. Set to `FALSE` for quiet
  operation inside pipelines.

- precip_accum:

  logical; how to interpret `MET_pprain` / `MET_ppsnow` when the data is
  sub-daily. `TRUE` (default) treats them as the depth accumulated
  *within each step* (the ERA5 / AWS convention) and rescales to the
  mm/day rate the rest of AEME expects; `FALSE` takes the values to be a
  mm/day rate already and leaves them untouched. Ignored for daily data,
  where the two are identical.

- tz:

  character; Olson timezone in which a naive/character `Date` column is
  expressed. Sub-daily timestamps are converted to UTC; daily data is
  treated as calendar dates and never shifted. A column that already
  carries a timezone (including `"UTC"`) is taken at face value here –
  reinterpreting a `"UTC"`-tagged column against a declared local zone
  happens once, upstream, in
  [`add_met`](https://limnotrack.com/reference/add_met.md) /
  [`aeme_constructor`](https://limnotrack.com/reference/aeme_constructor.md).
  Default `"UTC"`;
  [`build_aeme`](https://limnotrack.com/reference/build_aeme.md) passes
  the object's `time$tz`.

- longitude:

  numeric; lake longitude in decimal degrees (east positive). When
  supplied and the data is sub-daily, the hour at which `MET_radswd`
  peaks each day is compared against astronomical solar noon for that
  longitude; a warning is emitted if they differ by more than 3 h, which
  usually means the timestamps are in local time rather than UTC. `NULL`
  (default) skips the check.
  [`build_aeme`](https://limnotrack.com/reference/build_aeme.md) passes
  the lake longitude.

## Value

The input data frame with column names remapped to AEME standard names
and values converted to AEME standard units where a conversion was
necessary. Columns that could not be matched are retained unchanged with
a warning. A warning is also emitted if any required variable
(`MET_radswd`, `MET_tmpair`, `MET_wndspd`, `MET_pprain`) is absent after
renaming.

## AEME standard variables and units

|                     |              |                   |                    |
|---------------------|--------------|-------------------|--------------------|
| **Variable**        | **Name**     | **Unit**          | **Required**       |
| Shortwave radiation | `MET_radswd` | W/m²              | Yes                |
| Air temperature     | `MET_tmpair` | °C                | Yes                |
| Wind speed          | `MET_wndspd` | m/s               | Yes                |
| Rainfall            | `MET_pprain` | mm/day            | Yes                |
| Snowfall            | `MET_ppsnow` | mm/day            | No (defaults to 0) |
| u wind component    | `MET_wnduvu` | m/s               | No (derivable)     |
| v wind component    | `MET_wnduvv` | m/s               | No (derivable)     |
| Sea-level pressure  | `MET_prmslp` | Pa                | No (derivable)     |
| Station pressure    | `MET_prsttn` | Pa                | No (derivable)     |
| Cloud cover         | `MET_cldcvr` | 1 (fraction)      | No (derivable)     |
| Longwave radiation  | `MET_radlwd` | W/m²              | No (derivable)     |
| Dew point temp.     | `MET_tmpdew` | °C                | No (derivable)     |
| Vapour pressure     | `MET_prvapr` | hPa               | No (derivable)     |
| Relative humidity   | `MET_humrel` | \\ Wind direction | `MET_wnddir`       |
