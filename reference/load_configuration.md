# Load model configuration to the aeme object

Load model configuration to the aeme object

## Usage

``` r
load_configuration(
  aeme,
  model,
  path = ".",
  model_controls = NULL,
  use_bgc = FALSE,
  ext_elev = 0,
  calc_wbal = TRUE,
  wb_method = 2,
  calc_wlev = TRUE,
  use_aeme = FALSE,
  coeffs = NULL,
  hum_type = 3,
  est_swr_hr = TRUE
)
```

## Arguments

- aeme:

  aeme; object.

- model:

  vector; of models to be used. Can be \`dy_cd\`, \`glm_aed\`,
  \`gotm_wet\`.

- path:

  filepath; where input files are located relative to the current
  working directory.

- model_controls:

  dataframe; of configuration loaded from "model_controls.csv".

- use_bgc:

  logical; switch to use the biogeochemical model.

- ext_elev:

  numeric; metres to extend the hypograph by.

- calc_wbal:

  logical; calculate water balance. Default = TRUE.

- wb_method:

  numeric; method to use for calculating water balance. Must be 1 (no
  inflows or outflows) or 2 (outflows calculated) or 3 (Any unexplained
  gain in lake storage is treated as an effective inflow; any
  unexplained loss is treated as an effective outflow). Default = 2

- calc_wlev:

  logical; calculate water level.

- use_aeme:

  logical; use AEME object to generate model confiuration files.

- coeffs:

  numeric vector of length two; to be used to estimate surface water
  temperature for estimating evaporation. Defaults to NULL. If water
  temperature observations are included in \`aeme\` object, then it will
  use those to build a linear relationship between air temperature and
  water temperature. Otherwise. it uses the simple estimation
  \\temp_water = 5 + 0.75 \* temp_air\\ from Stefan & Preud'homme, 2007:
  www.doi.org/10.1111/j.1752-1688.1993.tb01502.x

- hum_type:

  numeric; GOTM humidity metric \[1=relative humidity ( 2=wet-bulb
  temperature, 3=dew point temperature, 4=specific humidity (kg/kg)\]
  Default = 3.

- est_swr_hr:

  logical; estimate hourly shortwave radiation from daily values.
  Default = TRUE.

## Value

Updated aeme object with model configuration
