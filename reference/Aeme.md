# Aeme Class

S4 Class representing AEME data

## Details

This class represents data related to a lake for running AEME. Items in
bold are required to run the models.

## Slots

- `lake`:

  A list representing lake information.

  - **`name`**: character; lake name.

  - **`id`**: character; lake identifier.

  - **`latitude`**: numeric; lake latitude.

  - **`longitude`**: numeric; lake longitude.

  - **`elevation`**: numeric; lake elevation.

  - **`depth`**: numeric; lake depth.

  - **`area`**: numeric; lake area.

- `time`:

  A list representing time information.

  - **`start`**: POSIXct (UTC); simulation start date-time.

  - **`stop`**: POSIXct (UTC); simulation stop date-time.

  - **`time_step`**: numeric; model integration time step in seconds.
    Default 3600 (1 hour).

  - **`output_time_step`**: numeric; model output time step in seconds.
    Must be \>= `time_step`. Default 86400 (daily).

  - **`output_daily_mean`**: logical; if `TRUE`, every model
    additionally produces a daily-mean output stream alongside its raw
    `output_time_step` output. Default `FALSE`. See
    [`set_output_time_step`](https://limnotrack.com/reference/set_output_time_step.md).

  - **`spin_up`**: list; spin up period in days for each model

  - `tz`: character; Olson timezone in which user-supplied timestamps
    (`start`, `stop`, and the date columns of meteo, inflow, outflow and
    observation inputs) are expressed. It is applied once, at ingest, to
    convert those timestamps to UTC; all datetimes are stored and
    computed in UTC internally. It is also used to render plots and
    summaries in local time. Defaults to `"UTC"`; set a non-UTC zone
    only when the source data is local time. Unrelated to GLM's numeric
    `timezone` nml parameter.

- `configuration`:

  A list representing each model's configuration.

  - `model_controls`: dataframe; Model controls for simulation.

  - `aeme_version`: character; version of the AEME package used to build
    the configuration.

  - `dy_cd`: list; DYRESM-CAEDYM configuration.

  - `glm_aed`: list; GLM-AED configuration.

  - `gotm_wet`: list; GOTM-WET configuration.

  - `simstrat_aed2`: list; Simstrat-AED2 configuration.

- `observations`:

  A list representing observation information.

  - `lake`: dataframe; lake observations. The `Date` column is a UTC
    `POSIXct`; daily observations are anchored at 12:00:00.

  - `level`: dataframe; lake level observations (`Date` as for `lake`).

- `input`:

  A list representing input information.

  - `init_profile`: dataframe; initial temperature profile (if none use
    NULL or leave empty; if empty/NULL, the observations file will be
    used).

  - **`init_depth`**: numeric; initial height of lake surface relative
    to the bottom (m).

  - **`hypsograph`**: dataframe; hypsograph.

  - **`meteo`**: dataframe; meteorological data.

  - **`use_lw`**: logical; use longwave radiation.

  - **`Kw`**: numeric; light extinction coefficient (m-1).

- `inflows`:

  A list representing inflows information.

  - `data`: named list of inflow dataframes.

  - `factor`: named list; inflow factors for each model.

- `outflows`:

  A list representing outflows information.

  - `data`: named list of outflow dataframes.

  - `factor`: named list; outflow factors for each model.

  - `lvl`: numeric; height of lake level outflow.

- `water_balance`:

  A list representing water balance information.

  - **`method`**: integer; Method for calculating water balance. 1 =
    none, 2 = outflows, 3 = inflows and outflows.

  - **`use`**: character; Can be 'obs' or 'mod'. Use observations or
    modelled data for water balance.

  - `data`: list of dataframe for water balance.

  - `params`: fitted outflow parameters (C, h_inv), keyed by evaporation
    family since `dy_cd`/`glm_aed` share one fit and
    `gotm_wet`/`simstrat_aed2` each have their own – see
    [`get_wbal_param`](https://limnotrack.com/reference/get_wbal_param.md).

- `output`:

  A list representing output information.

  - `dy_cd`: list; DYRESM-CAEDYM output.

  - `glm_aed`: list; GLM-AED output.

  - `gotm_wet`: list; GOTM-WET output.

  - `simstrat_aed2`: list; Simstrat-AED2 output.

- `parameters`:

  A dataframe representing model parameters: updates (e.g. calibrated
  values) applied on top of the model `configuration` when the model
  files are written. `configuration` itself is not changed by them; see
  [`effective_configuration()`](https://limnotrack.com/reference/effective_configuration.md)
  for the two combined.
