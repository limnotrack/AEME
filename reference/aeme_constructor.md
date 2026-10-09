# Constructor function for Aeme class

Constructor function for Aeme class

## Usage

``` r
aeme_constructor(
  lake,
  time,
  configuration,
  observations,
  input,
  inflows,
  outflows,
  water_balance,
  output,
  parameters,
  print = TRUE,
  tz = NULL
)
```

## Arguments

- lake:

  List representing lake information.

- time:

  List representing time information.

- configuration:

  List representing configuration information.

- observations:

  List representing observation information.

- input:

  List representing input information.

- inflows:

  List representing inflows information.

- outflows:

  List representing outflows information.

- water_balance:

  List representing water balance information.

- output:

  List representing output information.

- parameters:

  Dataframe containing model parameters.

- print:

  Logical; print messages. Default is TRUE.

- tz:

  character; Olson timezone in which user-supplied timestamps
  (`time$start`, `time$stop`, and the date columns of meteo, inflow,
  outflow and observation inputs) are expressed. Applied once, at
  ingest, to convert those timestamps to UTC; all datetimes are stored
  and computed in UTC internally, and `tz` is also used for display.
  Defaults to `time$tz` if present, otherwise `"UTC"`. Set a non-UTC
  zone only when your source data really is in local time (gridded
  reanalysis such as ERA5 is UTC). Unrelated to GLM's numeric `timezone`
  nml parameter.

## Value

An instance of the Aeme class.
