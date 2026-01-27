# Estimate Lake Water Levels with Nudging

This function estimates lake water levels using a hydrological model
with nudging to observed water levels. It optimizes parameters to
minimize the error between simulated and observed levels, applying
nudging to guide the simulation towards observed values.

## Usage

``` r
estimate_lake_wlev(
  data,
  hyps_df,
  model,
  init_elev,
  params = NULL,
  initial_guess = NULL,
  verbose = FALSE
)
```

## Arguments

- data:

  data frame with columns: - Date: Date of observation - HYD_flow:
  Inflow to the lake (m3/day) - MET_pprain: Precipitation on the lake
  surface (m/day) - evap_m3: Evaporation from the lake surface
  (m3/day) - lvl_obs: Observed lake water level (m) - is_obs_lvl:
  Logical indicating if lvl_obs is an observation (TRUE/FALSE)

- hyps_df:

  data frame with hypsograph data containing columns: - elev:
  Elevation (m) - area: Surface area at that elevation (m2) - volume:
  Volume at that elevation (m3)

- model:

  Character string indicating the evaporation model to use: - "dy_cd":
  DYRESM-CAEDYM dynamic evaporation - "glm_aed": GLM-AED dynamic
  evaporation - "gotm_wet": GOTM-WET dynamic evaporation

- initial_guess:

  Optional initial guess for optimization parameters: - C: Outflow
  coefficient - h_inv: Inversion height for outflow calculation

- verbose:

  Logical indicating whether to print optimization details

## Value

A data frame with original data and additional columns: - lvl_sim:
Simulated lake water level (m) - HYD_outflow_sim: Simulated lake outflow
(m3/day)
