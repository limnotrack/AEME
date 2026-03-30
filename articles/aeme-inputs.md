# AEME Inputs

## Summary

Inputs for AEME follow standardised conventions that are compatible with
most lake ecosystem modelling frameworks. All variable names are defined
in the `key_naming` data frame, which maps human-readable names to AEME
variable codes and their units.

## Lake

The `lake` list object provides physical and geographic metadata about
the waterbody. Required fields are:

| Field       | Type                | Description                           |
|-------------|---------------------|---------------------------------------|
| `name`      | character           | Name of the lake or reservoir         |
| `id`        | character / numeric | Unique identifier                     |
| `latitude`  | numeric             | Latitude (degrees North)              |
| `longitude` | numeric             | Longitude (degrees East)              |
| `elevation` | numeric             | Surface elevation above sea level (m) |
| `depth`     | numeric             | Maximum water depth (m)               |
| `area`      | numeric             | Surface area at maximum depth (m²)    |

The `depth` and `area` fields are used to generate a simple cone-shaped
hypsograph if none is provided in the `input` slot. For more accurate
simulations it is strongly recommended to supply a measured hypsograph.

## Meteorological Data

Meteorological data requirements for lake modelling are similar to those
used in most lake ecosystem models. The following meteorological
variables are **required** for AEME:

- Air temperature
- Shortwave radiation
- Wind speed
- Rain
- Either relative humidity or dew point temperature
- Either mean sea level pressure or station pressure

The following meteorological variables can be **derived** from the
required variables:

- Dew point temperature
- Relative humidity
- Precipitation vapor pressure
- Wind u and v components
- Cloud cover
- Longwave radiation

## Inflows and Outflows

### Inflows

Inflows supply water, heat, and (if the biogeochemistry module is
active) nutrients and sediment to the lake. Inflows are specified as a
**named list** of data frames passed to
[`add_inflows()`](https://limnotrack.com/reference/add_inflows.md). The
list name is used as the stream identifier in each model.

Each inflow data frame must contain:

| Column     | Units        | Description                                                      |
|------------|--------------|------------------------------------------------------------------|
| `Date`     | `YYYY-MM-DD` | Date of the observation                                          |
| `HYD_flow` | m³ day⁻¹     | Volumetric flow rate                                             |
| `HYD_temp` | °C           | Inflow water temperature (defaults to air temperature if absent) |
| `CHM_salt` | PSU          | Salinity (0 for freshwater)                                      |

Additional biogeochemical variables (e.g. `CHM_oxy`, `PHS_frp`,
`NIT_amm`) can be included if the ecosystem model is being used. All
variable names must match those in the `key_naming` data frame.

``` r
# Example: two inflows
inf_data <- list(
  stream_north = data.frame(
    Date     = seq(as.Date("2020-01-01"), as.Date("2021-12-31"), by = "day"),
    HYD_flow = 5000,
    HYD_temp = 12,
    CHM_salt = 0
  ),
  stream_south = data.frame(
    Date     = seq(as.Date("2020-01-01"), as.Date("2021-12-31"), by = "day"),
    HYD_flow = 3000,
    HYD_temp = 10,
    CHM_salt = 0
  )
)

aeme <- add_inflows(aeme, data = inf_data)
```

### Outflows

Outflows drain water from the lake or reservoir. Like inflows, they are
specified as a **named list** of data frames passed to
[`add_outflows()`](https://limnotrack.com/reference/add_outflows.md).

Each outflow data frame must contain:

| Column     | Units        | Description          |
|------------|--------------|----------------------|
| `Date`     | `YYYY-MM-DD` | Date                 |
| `HYD_flow` | m³ day⁻¹     | Volumetric flow rate |

#### Outlet elevation

The `elevation` argument of
[`add_outflows()`](https://limnotrack.com/reference/add_outflows.md)
specifies the depth at which each outlet withdraws water:

- Set `elevation = -1` (or `elevation = list(outflow = -1)` for a named
  list) for a **surface outlet** – the outlet always withdraws from the
  current water surface.
- Set `elevation` to the actual elevation **above sea level (m)** for a
  **fixed-depth outlet** (e.g. a dam penstock or selective withdrawal).
  The value must fall within the elevation range of the hypsograph.

``` r
# Single surface outlet
outf_single <- list(
  outlet = data.frame(
    Date     = seq(as.Date("2020-01-01"), as.Date("2021-12-31"), by = "day"),
    HYD_flow = 8000
  )
)

aeme <- add_outflows(
  aeme,
  data      = outf_single,
  elevation = list(outlet = -1)  # surface outlet
)
```

#### Multiple outlets at different levels

Reservoirs commonly have several outlets at different depths. Provide
one entry per outlet in both `data` and `elevation`:

``` r
# Reservoir with a surface spillway and a deep penstock
outf_multi <- list(
  spillway = data.frame(
    Date     = seq(as.Date("2020-01-01"), as.Date("2021-12-31"), by = "day"),
    HYD_flow = 2000  # only active during high water events
  ),
  penstock = data.frame(
    Date     = seq(as.Date("2020-01-01"), as.Date("2021-12-31"), by = "day"),
    HYD_flow = 8000  # daily regulated release
  )
)

# Reservoir with full supply level at 150 m a.s.l. and max depth 25 m
aeme <- add_outflows(
  aeme,
  data = outf_multi,
  elevation = list(
    spillway = -1,   # surface overflow
    penstock = 130   # 20 m below the full supply level
  )
)
```

For a worked example of a reservoir with two outlets, see
`vignette("reservoir-aeme")`.

## Parameters

Being able to specify the parameters of the model is crucial for the
simulation. Parameters can be stored in a data frame within the `aeme`
object. The parameters in this data frame are used to update the model
configuration files and/or update the meteorological data (e.g. scaling
wind factor) and/or update the inflow/outflow data (e.g. scaling inflow
factor).

``` r
utils::data("aeme_parameters")
aeme_parameters
```

``` r
parameters(aeme) <- aeme_parameters
```
