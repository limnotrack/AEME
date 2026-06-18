# AEME Inputs

## Summary

Inputs for AEME follow standardised conventions that are compatible with
most lake ecosystem modelling frameworks. All variable names are defined
in the `key_naming` data frame, which maps human-readable names to AEME
variable codes and their units.

This vignette provides a comprehensive reference for all AEME inputs,
organized by the slots of the S4 `Aeme` object. Understanding this
structure is essential for preparing data and configuring models. For a
high-level overview of AEME and the ensemble approach, see the
[Introduction to AEME](https://limnotrack.com/articles/intro-aeme.md)
vignette. For a practical tutorial on setting up a new lake, see
[Setting up AEME for a new
lake](https://limnotrack.com/articles/setup-new-lake.md).

## AEME S4 Structure

The `Aeme` object is an S4 class that contains all information required
to run the model ensemble. The object has ten slots, each containing
specific types of input data or model configuration:

1.  **lake** - Lake metadata (location, dimensions, identification)
2.  **time** - Temporal simulation settings (start, stop, timestep,
    spin-up)
3.  **configuration** - Model configurations and controls
4.  **parameters** - Model parameter values for calibration
5.  **observations** - Observational data for validation and
    initialization
6.  **input** - Core inputs (meteorology, hypsograph, light extinction)
7.  **inflows** - Stream inflow data and scaling factors
8.  **outflows** - Outlet data, elevations, and scaling factors
9.  **water_balance** - Water balance configuration and calculations
10. **output** - Model output (populated after running models)

Each of these slots is described in detail below.

## Lake Slot

The `lake` slot is a list object that provides physical and geographic
metadata about the waterbody. This information is used for model
initialization, spatial calculations (e.g., solar radiation), and
metadata tracking.

### Required Fields

All fields marked with **bold** are required:

| Field | Type | Required | Description |
|----|----|----|----|
| **`name`** | character | Yes | Name of the lake or reservoir |
| **`id`** | character/numeric | Yes | Unique identifier (e.g., database ID) |
| **`latitude`** | numeric | Yes | Latitude in decimal degrees (North positive) |
| **`longitude`** | numeric | Yes | Longitude in decimal degrees (East positive) |
| **`elevation`** | numeric | Yes | Surface elevation above sea level (m) |
| **`depth`** | numeric | Yes | Maximum water depth (m) |
| **`area`** | numeric | Yes | Surface area at full depth (m²) |
| `shape` | sf object | No | Lake polygon (used to calculate lat/lon if not provided) |

### How to Obtain Lake Data

- **Location (lat/lon)**: GPS measurements, online map tools, or
  centroid of shapefile
- **Elevation**: Digital elevation models (DEM), topographic maps, or
  lake databases. In New Zealand, use the LINZ DEM (see [setup
  vignette](https://limnotrack.com/articles/setup-new-lake.html#elevation-data))
- **Depth**: Bathymetric surveys, lake databases, or published
  literature
- **Area**: Calculated from shapefile using
  [`sf::st_area()`](https://r-spatial.github.io/sf/reference/geos_measures.html),
  satellite imagery, or lake databases

### Validation

The
[`aeme_constructor()`](https://limnotrack.com/reference/aeme_constructor.md)
function validates that: - Latitude is between -90 and 90 degrees -
Longitude is between -180 and 180 degrees  
- Depth and area are positive numbers - Elevation is numeric

### Notes

The `depth` and `area` fields are used to generate a simple cone-shaped
hypsograph if none is provided in the `input` slot. For more accurate
simulations it is **strongly recommended** to supply a measured
hypsograph (see [Input Slot](#input-slot)).

## Time Slot

The `time` slot is a list object that defines the temporal extent and
resolution of the simulation. It also specifies spin-up periods for
model initialization.

### Required Fields

| Field | Type | Required | Description |
|----|----|----|----|
| **`start`** | character/POSIXct | Yes | Start date and time of simulation |
| **`stop`** | character/POSIXct | Yes | End date and time of simulation |
| `timestep` | numeric | No | Model integration timestep in seconds (default: 3600) |
| `spin_up` | list | No | Named list of spin-up days for each model |

### Date Format

Dates must be provided in the format `YYYY-MM-DD HH:MM:SS` (e.g.,
`"2020-01-01 00:00:00"`). If only the date is provided, the time
defaults to midnight (00:00:00).

``` r

time <- list(
  start = "2020-01-01 00:00:00",
  stop  = "2021-12-31 23:00:00",
  timestep = 3600  # 1 hour in seconds
)
```

### Spin-up Period

The spin-up period allows models to initialize and reach equilibrium
before the main simulation period. During spin-up, models run but output
is discarded. This is especially important for water quality models.

Spin-up is specified as a named list with the number of days for each
model:

``` r

time <- list(
  start = "2020-01-01 00:00:00",
  stop  = "2021-12-31 23:00:00",
  spin_up = list(
    dy_cd = 30,      # DYRESM-CAEDYM: 30 days
    glm_aed = 30,    # GLM-AED: 30 days
    gotm_wet = 10    # GOTM-WET: 10 days
  )
)
```

If spin-up is not specified, default values are used (typically 0-2
days).

### Timestep

The `timestep` defines the temporal resolution of the simulation in
seconds: - **3600** (1 hour) - Standard for most applications - **1800**
(30 minutes) - Higher resolution for short-term dynamics - **86400** (1
day) - Coarser resolution for long-term simulations

Note that each model may internally adjust the timestep based on
stability requirements.

### Time Zones

All dates should be in the local time zone of the lake. The models do
not explicitly handle time zones but use solar position calculations
based on latitude and longitude.

### Time Zones

All dates should be in the local time zone of the lake. The models do
not explicitly handle time zones but use solar position calculations
based on latitude and longitude.

## Input Slot

The `input` slot is a list containing the core physical inputs required
by all models: meteorological forcing, lake hypsography, light
extinction, and initial conditions.

### Required Fields

| Field | Type | Required | Description |
|----|----|----|----|
| **`hypsograph`** | data.frame | Yes | Elevation-area-depth relationship |
| **`meteo`** | data.frame | Yes | Meteorological forcing data |
| **`Kw`** | numeric | Yes | Light extinction coefficient (m⁻¹) |
| `init_profile` | data.frame | No | Initial temperature and salinity profile |
| `init_depth` | numeric | No | Initial water depth (m; defaults to max depth) |
| `use_lw` | logical | No | Use longwave radiation (default: TRUE) |

### Hypsograph

The hypsograph defines the lake bathymetry as a relationship between
elevation, depth, and surface area. It **must** contain three columns:

| Column  | Units | Description                               |
|---------|-------|-------------------------------------------|
| `elev`  | m     | Elevation above sea level                 |
| `depth` | m     | Depth below surface (negative values)     |
| `area`  | m²    | Horizontal surface area at this elevation |

The hypsograph must span from the lake bottom to above the maximum
expected water level.

``` r

# Example hypsograph for a 10m deep lake at 100m elevation
hypsograph <- data.frame(
  elev  = c(105, 100, 95, 90),       # Surface to bottom
  depth = c(5, 0, -5, -10),          # Depths (negative below surface)
  area  = c(200000, 150000, 80000, 0)  # Areas (m²)
)
```

If you don’t have measured bathymetry, you can generate an idealized
hypsograph using
[`generate_hypsograph()`](https://limnotrack.com/reference/generate_hypsograph.md):

``` r

hypsograph <- generate_hypsograph(
  max_depth = 10,              # Maximum depth (m)
  surface_area = 150000,       # Surface area (m²)
  elev = 100,                  # Surface elevation (m)
  volume_development = 1.0,    # Shape factor (1.0 = cone)
  ext_elev = 5                 # Extend above surface (m)
)
```

The `volume_development` parameter controls the hypsograph shape: - **\<
1.0**: Concave (bowl-shaped) - **= 1.0**: Linear (cone-shaped)  
- **\> 1.0**: Convex (saucer-shaped)

### Initial Conditions

**Initial Temperature and Salinity Profile (`init_profile`)**

If provided, must be a data frame with columns `depth`, `temperature`,
and `salt`:

``` r

init_profile <- data.frame(
  depth = c(0, 5, 10),         # Depth below surface (m)
  temperature = c(18, 15, 10), # Temperature (°C)
  salt = c(0, 0, 0)            # Salinity (PSU; 0 for freshwater)
)
```

If `init_profile` is not provided, AEME automatically generates one
using: 1. Observations from the `observations` slot (if available) 2.
Default values from `model_controls` (see [Configuration
Slot](#configuration-slot))

**Initial Water Depth (`init_depth`)**

The initial water level relative to the lake bottom. If not provided,
defaults to the maximum depth from the hypsograph. This should
correspond to an elevation within the hypsograph range.

### Light Extinction Coefficient (`Kw`)

The light extinction coefficient (m⁻¹) determines how quickly light
attenuates with depth. Typical values:

- **0.1 - 0.5**: Very clear oligotrophic lakes
- **0.5 - 2.0**: Mesotrophic lakes (moderate clarity)  
- **2.0 - 10**: Eutrophic lakes (high algae or dissolved organic matter)
- **\> 10**: Highly turbid or dystrophic systems

If you don’t have measured values, estimate from Secchi depth:

Kw ≈ 1.7 / Secchi_depth

### Longwave Radiation (`use_lw`)

If `use_lw = TRUE` (default), the models use longwave radiation from the
meteorological data or estimate it if not provided. If `use_lw = FALSE`,
longwave is estimated entirely from cloud cover and air temperature.

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

## Inflows Slot

### Inflows

Inflows supply water, heat, and (if the biogeochemistry module is
active) nutrients and sediment to the lake. Inflows are specified as a
**named list** of data frames passed to
[`add_inflows()`](https://limnotrack.com/reference/add_inflows.md). The
list name is used as the stream identifier in each model.

Each inflow data frame must contain:

| Column | Units | Description |
|----|----|----|
| `Date` | `YYYY-MM-DD` | Date of the observation |
| `HYD_flow` | m³ day⁻¹ | Volumetric flow rate |
| `HYD_temp` | °C | Inflow water temperature (defaults to air temperature if absent) |
| `CHM_salt` | PSU | Salinity (0 for freshwater) |

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

**Inflow Factors**

The `inflows` slot also includes a `factor` component for model-specific
scaling:

``` r

# Different scaling for each model
inf_factors <- list(
  dy_cd = 1.2,     # Increase DYRESM inflows by 20%
  glm_aed = 1.0,   # No change for GLM
  gotm_wet = 0.9   # Decrease GOTM inflows by 10%
)

aeme <- add_inflows(aeme, data = inf_data, factor = inf_factors)
```

Factors can also be specified in the `parameters` slot for calibration.

**Water Balance Integration**

When `water_balance$method = 3`, calculated inflows are added to the
`inflows` slot as `"wbal"`. These are model-specific estimates based on
water level change, evaporation, and outflows.

## Outflows Slot

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

## Observations Slot

The `observations` slot is a list containing observational data used for
model validation, calibration, and initialization. It has two
components: `lake` (in-lake observations) and `level` (water level
observations).

### Lake Observations (`observations$lake`)

In-lake observations are stored as a data frame with the following
structure:

| Column | Type | Description |
|----|----|----|
| `Date` | character/POSIXct | Observation date and time (`YYYY-MM-DD HH:MM:SS`) |
| `depth` | numeric | Depth below surface (m) |
| `var` | character | Variable name (must match `key_naming$var_aeme`) |
| `value` | numeric | Observed value |

``` r

lake_obs <- data.frame(
  Date  = c("2020-06-15 10:00:00", "2020-06-15 10:00:00", "2020-06-15 10:00:00"),
  depth = c(0, 5, 10),
  var   = c("HYD_temp", "HYD_temp", "HYD_temp"),
  value = c(18.5, 15.2, 10.8)
)
```

**Variable Names**

All variable names must match those defined in the `key_naming` data
frame. Common variables include:

- `HYD_temp`: Water temperature (°C)
- `CHM_oxy`: Dissolved oxygen (mg/L or mmol/m³)
- `PHY_tchla`: Total chlorophyll-a (µg/L)
- `NIT_tn`: Total nitrogen (mg/L)
- `PHS_tp`: Total phosphorus (mg/L)

View all available variables:

``` r

data("key_naming", package = "AEME")
View(key_naming)
```

### Water Level Observations (`observations$level`)

Water level observations are stored as a data frame:

| Column  | Type              | Description                               |
|---------|-------------------|-------------------------------------------|
| `Date`  | character/POSIXct | Observation date and time                 |
| `value` | numeric           | Water level elevation (m above sea level) |

``` r

level_obs <- data.frame(
  Date  = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day"),
  value = 100 + rnorm(366, mean = 0, sd = 0.5)  # Around 100m elevation
)
```

**Important**: Water level values must be: - In metres above sea level -
Within the range of the hypsograph elevations

### How Observations Are Used

1.  **Model Validation**: The
    [`assess_model()`](https://limnotrack.com/reference/assess_model.md)
    function compares model output to observations and calculates
    performance metrics (RMSE, NSE, etc.)

2.  **Calibration**: The
    [aemetools](https://github.com/limnotrack/aemetools) package uses
    observations as targets for automated calibration

3.  **Initialization**: If `init_profile` is not provided in the `input`
    slot, AEME uses lake observations near the start date to initialize
    temperature and water quality

4.  **Water Balance**: Water level observations are used to calculate
    inflows and outflows when `water_balance$method` is 2 or 3 (see
    [Water Balance Slot](#water-balance-slot))

## Water Balance Slot

The `water_balance` slot is generated internally by
[`build_aeme()`](https://limnotrack.com/reference/build_aeme.md) when
the `wb_method` argument is set to 2 or 3. It stores configuration and
calculated water balance components.

### Structure

| Field    | Type      | Description                         |
|----------|-----------|-------------------------------------|
| `method` | integer   | Water balance method (1, 2, or 3)   |
| `use`    | character | Data source: `"obs"` or `"mod"`     |
| `data`   | list      | Calculated water balance components |

### Water Balance Methods

**Method 1: No Water Balance** - Inflows and outflows are provided
directly by the user - No automatic calculations - Use when you have
complete inflow/outflow data

**Method 2: Calculate Outflows** - Inflows are provided by the user -
Outflows are calculated from water level change, evaporation, and
inflows - Requires water level observations (`observations$level`) - Use
for natural lakes with ungauged outflows

**Method 3: Calculate Both Inflows and Outflows**  
- Both inflows and outflows are calculated from water balance - Requires
water level observations - Use for closed basins or when both inflow and
outflow are unknown

### Data Source (`use`)

- **`"obs"`**: Use observed water levels for calculations (recommended)
- **`"mod"`**: Use modeled water levels (can lead to circular
  dependencies)

### Calculated Components (`data`)

When [`build_aeme()`](https://limnotrack.com/reference/build_aeme.md)
runs with `wb_method = 2` or `3`, it populates `data` with:

- `wbal`: Data frame with water balance diagnostics for each model
  - Evaporation (calculated differently by each model)
  - Water level change
  - Estimated inflows/outflows
- `model`: Modeled water level (if `use = "mod"`)

### Example

``` r

aeme <- build_aeme(
  aeme = aeme,
  model = c("dy_cd", "glm_aed", "gotm_wet"),
  model_controls = model_controls,
  path = "my_lake",
  wb_method = 2,  # Calculate outflows
  use_wbal = "obs"  # Use observed water levels
)

# Access water balance data
wbal_data <- water_balance(aeme)
str(wbal_data)
```

For a detailed example, see `vignette("rotoehu-water-balance")`.

## Configuration Slot

The `configuration` slot stores model-specific configuration files and
the `model_controls` data frame. This slot is populated when you run
[`build_aeme()`](https://limnotrack.com/reference/build_aeme.md).

### Structure

The `configuration` slot contains:

- `model_controls`: Data frame defining which variables to simulate
- `dy_cd`: List of DYRESM-CAEDYM configuration files
- `glm_aed`: List of GLM-AED configuration files  
- `gotm_wet`: List of GOTM-WET configuration files

### Model Controls

The `model_controls` data frame defines which variables to simulate and
their default values. Generate it using:

``` r

# For hydrodynamics only
model_controls <- get_model_controls(use_bgc = FALSE)

# For hydrodynamics + biogeochemistry
model_controls <- get_model_controls(use_bgc = TRUE)
```

**Columns in `model_controls`**:

| Column           | Description                                      |
|------------------|--------------------------------------------------|
| `var_aeme`       | AEME variable name                               |
| `simulate`       | Logical; simulate this variable?                 |
| `inf_default`    | Default value in inflows if not provided         |
| `initial_wc`     | Initial value for water column                   |
| `initial_sed`    | Initial value for sediments (DYRESM-CAEDYM only) |
| `conversion_aed` | Unit conversion factor for GLM-AED               |

You can customize `model_controls` to add/remove variables:

``` r

# Load default controls
model_controls <- get_model_controls(use_bgc = TRUE)

# Remove a variable from simulation
model_controls$simulate[model_controls$var_aeme == "PHY_green"] <- FALSE

# Change default inflow concentration
model_controls$inf_default[model_controls$var_aeme == "NIT_nit"] <- 0.05
```

### Model Configuration Files

When [`build_aeme()`](https://limnotrack.com/reference/build_aeme.md)
runs, it creates model-specific configuration files based on the AEME
inputs:

**DYRESM-CAEDYM** - Hydrodynamic: `*.cfg`, `*.par` - Ecosystem: `*.con`,
`caedym3p1.bio`, `caedym3p1.chm`, `caedym3p1.sed`

**GLM-AED** - Hydrodynamic: `glm3.nml` - Ecosystem: `aed2.nml`,
`phytos.nml`, `zoops.nml`

**GOTM-WET** - Hydrodynamic: `gotm.yaml`, `output.yaml` - Ecosystem:
`fabm.yaml`

These files are stored as text in the `configuration` slot and can be
extracted:

``` r

# Get configuration
cfg <- configuration(aeme)

# Extract GLM configuration
glm_nml <- cfg$glm_aed$glm3.nml
cat(glm_nml)

# Modify and replace
# ... (advanced users only)
configuration(aeme)$glm_aed$glm3.nml <- modified_nml
```

For most users, it’s better to modify inputs in the AEME object rather
than directly editing configuration files.

## Parameters Slot

The `parameters` slot is a data frame containing model parameter values.
This is crucial for model calibration, sensitivity analysis, and
ensemble configuration. Parameters can modify model physics,
biogeochemistry, or scale meteorological and hydrological inputs.

### Structure

The parameters data frame has the following columns:

| Column | Type | Required | Description |
|----|----|----|----|
| `model` | character | Yes | Model name: `"dy_cd"`, `"glm_aed"`, or `"gotm_wet"` |
| `file` | character | Yes | Configuration file or input type |
| `name` | character | Yes | Parameter name (with path for nested parameters) |
| `value` | numeric | Yes | Parameter value |
| `min` | numeric | No | Minimum value for calibration |
| `max` | numeric | No | Maximum value for calibration |
| `group` | character | No | Phytoplankton group (GOTM-WET only) |
| `index` | integer | No | Vector index (GLM-AED only) |
| `module` | character | No | Model module name (for organization) |

### File Types

The `file` column specifies where the parameter is applied:

- **Model-specific files**: `"glm3.nml"`, `"gotm.yaml"`, `"*.cfg"`,
  `"aed2.nml"`, `"fabm.yaml"`, etc.
- **`"met"`**: Meteorological scaling (e.g., wind speed multiplier)
- **`"inf"`**: Inflow scaling (e.g., flow multiplier)
- **`"wdr"`**: Outflow/withdrawal scaling

### Parameter Names

For parameters nested in configuration files, use `/` to separate
hierarchy levels:

``` r

# GLM-AED example
params <- data.frame(
  model = "glm_aed",
  file  = "glm3.nml",
  name  = c("light/Kw", "mixing/coef_mix_hyp"),
  value = c(1.5, 0.5),
  min   = c(0.1, 0.1),
  max   = c(5.0, 2.0)
)
```

### Examples

**1. Physical Parameters**

``` r

physical_params <- data.frame(
  model = c("glm_aed", "glm_aed", "gotm_wet"),
  file  = c("glm3.nml", "glm3.nml", "gotm.yaml"),
  name  = c("light/Kw", "mixing/coef_mix_hyp", "turbulence/turb_param/k_min"),
  value = c(1.2, 0.5, 5e-6),
  min   = c(0.5, 0.1, 1e-6),
  max   = c(3.0, 2.0, 1e-5),
  module = c("hydrodynamic", "hydrodynamic", "hydrodynamic")
)
```

**2. Biogeochemical Parameters**

``` r

bgc_params <- data.frame(
  model = c("glm_aed", "glm_aed"),
  file  = c("aed.nml", "aed.nml"),
  name  = c("aed_oxygen/fsed_oxy", "aed_nitrogen/rnitrif"),
  value = c(-10, 0.05),
  min   = c(-50, 0.01),
  max   = c(-5, 0.2),
  module = c("oxygen", "nitrogen")
)
```

**3. Meteorological Scaling**

``` r

met_params <- data.frame(
  model = c("glm_aed", "dy_cd", "gotm_wet"),
  file  = "met",
  name  = c("MET_wndspd", "MET_wndspd", "MET_wndspd"),
  value = c(1.2, 1.2, 1.2),  # 20% increase in wind speed
  min   = c(0.8, 0.8, 0.8),
  max   = c(1.5, 1.5, 1.5)
)
```

**4. Inflow/Outflow Scaling**

``` r

hydro_params <- data.frame(
  model = c("glm_aed", "glm_aed"),
  file  = c("inf", "wdr"),
  name  = c("inflow", "outflow"),
  value = c(1.0, 1.0),
  min   = c(0.5, 0.5),
  max   = c(2.0, 2.0)
)
```

### Using Parameters

Add parameters to an AEME object:

``` r

# Load example parameters
data("aeme_parameters", package = "AEME")

# Add to AEME object
parameters(aeme) <- aeme_parameters

# Or build with parameters
aeme <- build_aeme(
  aeme = aeme,
  model = c("dy_cd", "glm_aed", "gotm_wet"),
  model_controls = model_controls,
  path = "my_lake"
)
# Then add parameters
parameters(aeme) <- my_params
```

### Retrieving Parameters

Use
[`get_aeme_parameters()`](https://limnotrack.com/reference/get_aeme_parameters.md)
to extract parameters by model or module:

``` r

# Get all parameters
all_params <- get_aeme_parameters(aeme)

# Get GLM-AED parameters only
glm_params <- get_aeme_parameters(aeme, model = "glm_aed")

# Get parameters for specific module
mixing_params <- get_aeme_parameters(aeme, module = "Mixing")
```

### Integration with Calibration

The `min` and `max` columns define parameter ranges for calibration
using the [aemetools](https://github.com/limnotrack/aemetools) package:

``` r

library(aemetools)

# Calibrate using parameters in aeme object
calib <- calib_aeme(
  aeme = aeme,
  model = "glm_aed",
  vars_sim = c("HYD_temp", "CHM_oxy")
)
```

See the [aemetools
documentation](https://limnotrack.github.io/aemetools/) for details on
calibration and sensitivity analysis.

## Output Slot

The `output` slot stores model results after running
[`run_aeme()`](https://limnotrack.com/reference/run_aeme.md). Initially
empty, it is populated with time-series data for each model and
variable.

### Structure

The `output` slot is a list with one element per model:

- `dy_cd`: DYRESM-CAEDYM output
- `glm_aed`: GLM-AED output
- `gotm_wet`: GOTM-WET output

Each model element is itself a list containing data frames for each
simulated variable.

### Running Models

``` r

# Run all models in the ensemble
aeme <- run_aeme(aeme = aeme, model = c("dy_cd", "glm_aed", "gotm_wet"))

# Run specific model(s)
aeme <- run_aeme(aeme = aeme, model = "glm_aed")
```

### Accessing Output

Extract output using the
[`output()`](https://limnotrack.com/reference/output.md) function:

``` r

# Get all output
all_output <- output(aeme)

# Get output for specific model
glm_output <- all_output$glm_aed

# Get specific variable
temp_data <- glm_output$HYD_temp
head(temp_data)
```

### Output Structure

Each variable’s output is a data frame in long format:

| Column  | Description             |
|---------|-------------------------|
| `Date`  | Date and time           |
| `depth` | Depth below surface (m) |
| `value` | Variable value          |
| `model` | Model name              |
| `var`   | Variable name           |

``` r

# Example temperature output
#         Date  depth value    model      var
# 1 2020-01-01   0.0  18.5  glm_aed  HYD_temp
# 2 2020-01-01   1.0  18.3  glm_aed  HYD_temp
# 3 2020-01-01   2.0  17.9  glm_aed  HYD_temp
```

### Visualizing Output

Use [`plot_output()`](https://limnotrack.com/reference/plot_output.md)
to visualize results:

``` r

# Plot temperature from all models
plot_output(aeme = aeme, var_sim = "HYD_temp")

# Plot specific model
plot_output(aeme = aeme, model = "glm_aed", var_sim = "HYD_temp")

# Plot multiple variables
plot_output(aeme = aeme, var_sim = c("HYD_temp", "CHM_oxy"))

# Compare with observations
plot_output(aeme = aeme, var_sim = "HYD_temp", add_obs = TRUE)
```

### Model Comparison

Compare model performance using
[`assess_model()`](https://limnotrack.com/reference/assess_model.md):

``` r

# Compare all models to observations
assessment <- assess_model(aeme = aeme)
print(assessment)

# Metrics include:
# - RMSE: Root Mean Square Error
# - NSE: Nash-Sutcliffe Efficiency  
# - R²: Coefficient of determination
# - Bias: Mean bias
```

### Output Variables

The variables in the output depend on `model_controls`. Common variables
include:

**Hydrodynamic**: - `HYD_temp`: Water temperature (°C) - `LKE_lvlwtr`:
Water level (m) - `HYD_dens`: Water density (kg/m³)

**Water Quality**: - `CHM_oxy`: Dissolved oxygen (mg/L) - `PHY_tchla`:
Total chlorophyll-a (µg/L) - `NIT_tn`: Total nitrogen (mg/L) - `PHS_tp`:
Total phosphorus (mg/L)

View all simulated variables:

``` r

cfg <- configuration(aeme)
vars_simulated <- cfg$model_controls$var_aeme[cfg$model_controls$simulate]
print(vars_simulated)
```
