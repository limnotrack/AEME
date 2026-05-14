# Standardise meteorological variable names and units for AEME

Attempts to match column names in a meteorological data frame to AEME
standard variable names using
[`guess_aeme_vars()`](https://limnotrack.com/reference/guess_aeme_vars.md),
then detects the likely input units of each variable from its values and
converts to the units expected by the package.

## Usage

``` r
standardise_met(met, verbose = TRUE)
```

## Arguments

- met:

  data.frame; meteorological data with a `Date` column and one or more
  meteorological variable columns.

- verbose:

  logical; if `TRUE` (default), emit `cli_inform` messages describing
  each detected unit conversion applied. Set to `FALSE` for quiet
  operation inside pipelines.

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
