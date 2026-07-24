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

  - **`start`**: character; start date.

  - **`stop`**: character; end date.

  - **`timestep`**: numeric; time step.

  - **`spin_up`**: list; spin up information for each model

- `configuration`:

  A list representing each model's configuration.

  - `model_controls`: dataframe; Model controls for simulation.

  - `dy_cd`: list; DYRESM-CAEDYM configuration.

  - `glm_aed`: list; GLM-AED configuration.

  - `gotm_wet`: list; GOTM-WET configuration.

  - `simstrat_aed2`: list; Simstrat-AED2 configuration.

- `observations`:

  A list representing observation information.

  - `lake`: dataframe; lake observations.

  - `level`: dataframe; lake level observations.

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

- `output`:

  A list representing output information.

  - `dy_cd`: list; DYRESM-CAEDYM output.

  - `glm_aed`: list; GLM-AED output.

  - `gotm_wet`: list; GOTM-WET output.

  - `simstrat_aed2`: list; Simstrat-AED2 output.

- `parameters`:

  A dataframe representing model parameters.
