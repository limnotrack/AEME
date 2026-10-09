# GLM-AED: The General Lake Model coupled with AED

[⬇ Skip to Parameter Library](#parameter-library)

### Introduction

#### The General Lake Model (GLM)

The **General Lake Model (GLM)** is a one-dimensional (1-D),
variable-layer hydrodynamic model for lakes, reservoirs, and estuaries.
Originally described by Hipsey *et al.* (2019), GLM simulates the
vertical profiles of temperature, salinity, and density driven by
surface heat exchange, shortwave radiation, wind mixing, inflows, and
outflows. Its key architectural features include:

- **Variable-thickness layer scheme** — layers merge and split
  dynamically, giving high resolution where it is needed (near the
  thermocline) and coarser resolution elsewhere.
- **Flexible inflow/outflow** — supports multiple rivers, surface
  spillways, and subsurface outlets at arbitrary elevations.
- **Rich surface heat-flux options** — sensible heat, latent heat,
  longwave, and shortwave radiation computed from standard
  meteorological inputs.
- **Sediment heat exchange** — allows depth-varying bed temperatures to
  drive conductive heat flux into the water column.

There is more detailed GLM documentation available from the [GLM
website](https://aquatic.science.uwa.edu.au/research/models/GLM/),

Source code: :github:
<https://github.com/AquaticEcoDynamics/GLM/tree/master>

Binaries: :github: <https://github.com/AquaticEcoDynamics/Binaries>

##### References

Hipsey, M.R., Bruce, L.C., Boon, C., Busch, B., Carey, C.C., Hamilton,
D.P., Hanson, P.C., Read, J.S., de Sousa, E., Weber, M. & Winslow, L.A.
(2019). A General Lake Model (GLM 3.0) for linking with high-frequency
sensor data from the Global Lake Ecological Observatory Network (GLEON).
*Geoscientific Model Development*, 12, 473–523.
<https://doi.org/10.5194/gmd-12-473-2019>

------------------------------------------------------------------------

#### The Aquatic Ecosystem Dynamics (AED) library

The **Aquatic Ecodynamics (AED)** library (Hipsey *et al.*, 2013) is a
modular biogeochemical modelling framework designed to be coupled with
hydrodynamic models such as GLM. Each biogeochemical process is
implemented as a self-contained *module* that can be switched on or off
independently, making it easy to build simulations that range from
simple oxygen tracking through to full
nutrient–phytoplankton–zooplankton food-web dynamics.

There is a detailed AED manual available from the [AED
website](https://aquaticecodynamics.github.io/aed-science/), but here we
provide a brief overview of the key modules relevant for GLM-AED.

##### AED modules available in AEME

| Module | AED name | Description |
|----|----|----|
| Sediment flux | `aed_sedflux` | Sediment–water interface exchange of O₂, nutrients, and silica. Supports constant, constant-2D (depth-zone-specific), and dynamic flux models. |
| Oxygen | `aed_oxygen` | Dissolved-oxygen dynamics including reaeration, sediment oxygen demand (SOD), and photosynthesis/respiration coupling. |
| Silica | `aed_silica` | Reactive silica cycling — important for diatom growth. |
| Nitrogen | `aed_nitrogen` | Full dissolved inorganic nitrogen cycle: ammonium (NH₄⁺), nitrite (NO₂⁻), nitrate (NO₃⁻), N₂O; nitrification, denitrification, N-fixation. |
| Phosphorus | `aed_phosphorus` | Dissolved reactive phosphorus (DRP / FRP) including redox-sensitive sediment release. |
| Organic matter | `aed_organic_matter` | Particulate and dissolved organic carbon (POC/DOC) and organic nitrogen and phosphorus pools; mineralisation, decomposition. |
| Phytoplankton | `aed_phytoplankton` | Multi-group phytoplankton dynamics: growth, respiration, nutrient uptake, light limitation, settling, mortality. Default groups: cyanobacteria, green algae, diatoms. |
| Zooplankton | `aed_zooplankton` | Zooplankton grazing and dynamics. |
| Macrophytes | `aed_macrophyte` | Submerged and emergent macrophyte dynamics. |
| Totals | `aed_totals` | Diagnostic aggregates: TN, TP, TOC, chlorophyll-*a*. |

AED biogeochemical modules available in AEME. {.table}

#### GLM-AED Parameter Library

The `glm_aed_parameter_library` dataset provides a comprehensive list of
all parameters used in the GLM-AED configuration, including their
default values, units, and typical ranges. This library serves as a
reference for users to understand the parameters governing the model
behaviour and to guide parameterisation for specific applications. It
also includes metadata such as the associated AED module and a brief
description of each parameter’s role in the model and a web source link
for further information.

#### AED Phytoplankton Group Parameters

------------------------------------------------------------------------

### Getting started

``` r

library(AEME)
library(ggplot2)
library(dplyr)
```

We will use the example lake dataset bundled with the AEME package
throughout this vignette.

``` r

aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
aeme     <- readRDS(aeme_file)
aeme
```


    #>                                                                                 
    #> ── AEME v0.4.0 ─────────────────────────────────────────────────────────────────
    #>                                                                                 
    #> ── Lake ──                                                                      
    #>                                                                                 
    #> Wainamu (ID: LID45819)                                                          
    #> • Lat: -36.89; Lon: 174.47                                                      
    #> • Elev: 23.64m; Depth: 13.07m; Area: 152343 m2                                  
    #>                                                                                 
    #> ── Time ──                                                                      
    #>                                                                                 
    #> • Start: 2020-08-01 00:00:00; Stop: 2021-06-30 00:00:00; Time step: 3600 s;     
    #>   Output step: s                                                                
    #> • Timezone: UTC (timestamps stored UTC)                                         
    #> • Spin up (days): GLM: 2; GOTM: 1; DYRESM: 1; Simstrat: 2                       
    #>                                                                                 
    #> ── Configuration ──                                                             
    #>                                                                                 
    #> • Model: glm_aed                                                                
    #> • Path: C:/Users/mooret/Git/AEME                                                
    #> • Model controls: Present                                                       
    #> • Use biogeochemical model: No                                                  
    #> ┌ Model Configuration ─────────────────────────────────────────┐                
    #> │       Model              Physical         Biogeochemical     │                
    #> │ ---                                                          │                
    #> │       DY-CD               Absent              Absent         │                
    #> │      GLM-AED              Absent              Absent         │                
    #> │      GOTM-WET             Absent              Absent         │                
    #> │   SIMSTRAT-AED2           Absent              Absent         │                
    #> │    SIMSTRAT-AED           Absent              Absent         │                
    #> └──────────────────────────────────────────────────────────────┘                
    #>                                                                                 
    #> ── Observations ──                                                              
    #>                                                                                 
    #> • Lake: Present; Level: Present                                                 
    #>                                                                                 
    #> ── Input ──                                                                     
    #>                                                                                 
    #> • Initial profile: Absent; Initial depth: 13.07m                                
    #> • Hypsograph: Present (n=132)                                                   
    #> • Meteo: Present; Use longwave: TRUE; Kw: 1.31                                  
    #>                                                                                 
    #> ── Inflows ──                                                                   
    #>                                                                                 
    #> • Number of inflows: 1; Names: FWMT                                             
    #> • Scaling factors: DY-CD: 1; GLM-AED: 1; GOTM-WET: 1; Simstrat-AED2: 1          
    #>                                                                                 
    #> ── Outflows ──                                                                  
    #>                                                                                 
    #> • Number of outflows: 1; Names: outflow; Elevations: -1                         
    #> • Scaling factors: DY-CD: 1; GLM-AED: 1; GOTM-WET: 1; Simstrat-AED2: 1          
    #>                                                                                 
    #> ── Water Balance ──                                                             
    #>                                                                                 
    #> • Method: 2; Use: obs                                                           
    #> • Modelled: Absent; Water balance: Absent                                       
    #>                                                                                 
    #> ── Parameters ──                                                                
    #>                                                                                 
    #> • Number of parameters: 0                                                       
    #>                                                                                 
    #> ── Output ──                                                                    
    #>                                                                                 
    #> • DY-CD: 0                                                                      
    #> • GLM-AED: 0                                                                    
    #> • GOTM-WET: 0                                                                   
    #> • SIMSTRAT-AED2: 0                                                              
    #> • SIMSTRAT-AED: 0                                                               
    #> • Variables: 0                                                                  
    #> None                                                                            

------------------------------------------------------------------------

### Building a GLM-AED simulation

#### Model controls

[`get_model_controls()`](https://limnotrack.com/reference/get_model_controls.md)
returns a data frame that governs which biogeochemical variables are
simulated and how they are initialised. Pass `use_bgc = TRUE` to enable
the full suite of water-quality variables.

``` r

model_controls <- get_model_controls(use_bgc = TRUE)
head(model_controls)
#> <model_controls> 6/6 variables simulated
#>   var_aeme simulate inf_default initial_wc initial_sed conversion_aed
#>    CAR_doc      yes           0        0.5   1000000.0          0.012
#>    CAR_poc      yes           0        0.2         0.1          0.012
#>    CHM_oxy      yes          10       10.0        10.0          0.032
#>   CHM_salt      yes           0        0.0         0.0          1.000
#>   HYD_dens      yes           -          -           -          1.000
#>  HYD_strat      yes           -          -           -          1.000
```

You can narrow the set of simulated variables using
[`set_vars_sim()`](https://limnotrack.com/reference/set_vars_sim.md):

``` r

vars_sim <- c(
  "HYD_strat",   # stratification flag
  "HYD_temp",    # water temperature
  "HYD_thmcln",  # thermocline depth
  "CHM_oxy",     # dissolved oxygen
  "CHM_oxycln",  # oxycline depth
  "NIT_amm",     # ammonium
  "NIT_nit",     # nitrate
  "NIT_tn",      # total nitrogen
  "PHS_frp",     # filterable reactive phosphorus
  "PHS_tp",      # total phosphorus
  "PHY_tchla"    # total chlorophyll-a
)

model_controls <- set_vars_sim(
  model_controls = model_controls,
  vars_sim       = vars_sim
)
```

#### Build the model

We will build the model in a directory called `aeme`,

``` r

path <- "aeme"
```

[`build_aeme()`](https://limnotrack.com/reference/build_aeme.md)
translates the AEME object into all configuration files required by
GLM-AED. Setting `use_bgc = TRUE` writes the `aed/aed.nml` file and
supporting CSV parameter files for phytoplankton, zooplankton, and
macrophytes.

``` r

model <- "glm_aed"

aeme <- build_aeme(
  aeme           = aeme,
  model          = model,
  path           = path,
  model_controls = model_controls,
  ext_elev       = 3,
  use_bgc        = TRUE
)
```

The configuration files are stored in the `configuration` slot of the
`aeme` object. For GLM-AED the slot contains the parsed GLM hydrodynamic
nml (`glm3.nml` or `glm4.nml`, whichever version was built – see
[`find_glm_nml()`](https://limnotrack.com/reference/find_glm_nml.md))
and `aed/aed.nml`:

``` r

cfg <- configuration(aeme)
names(cfg[["glm_aed"]])
#> [1] "hydrodynamic"      "bgc"               "hydrodynamic_file"
```

------------------------------------------------------------------------

### GLM-AED specific features

#### Selecting AED biogeochemical modules

By default, all AED modules are enabled.
[`set_glm_aed_models()`](https://limnotrack.com/reference/set_glm_aed_models.md)
lets you choose a subset — for example, running hydrodynamics coupled
with oxygen and nutrient cycling *only*, without phytoplankton or
zooplankton:

``` r

# Rebuild first so we have a clean state
aeme <- build_aeme(
  aeme           = aeme,
  model          = model,
  model_controls = model_controls,
  path           = path,
  ext_elev       = 5,
  use_bgc        = TRUE
)

# Switch on all available modules (the default)
aeme <- set_glm_aed_models(
  aeme       = aeme,
  path       = path,
  aed_models = c(
    "aed_sedflux",
    "aed_oxygen",
    "aed_silica",
    "aed_nitrogen",
    "aed_phosphorus",
    "aed_organic_matter",
    "aed_phytoplankton",
    "aed_zooplankton",
    "aed_macrophyte",
    "aed_totals"
  )
)
```

To run a minimal simulation with only oxygen and nutrient cycling:

``` r

aeme <- set_glm_aed_models(
  aeme       = aeme,
  path       = path,
  aed_models = c(
    "aed_sedflux",
    "aed_oxygen",
    "aed_nitrogen",
    "aed_phosphorus"
  )
)
```

[`set_glm_aed_models()`](https://limnotrack.com/reference/set_glm_aed_models.md)
works on a model that has already been built. To choose the modules (and
which other AED setup steps run) as part of the build itself, pass
[`aed_options()`](https://limnotrack.com/reference/aed_options.md) to
[`build_aeme()`](https://limnotrack.com/reference/build_aeme.md):

``` r

aeme <- build_aeme(
  aeme           = aeme,
  model          = model,
  model_controls = model_controls,
  path           = path,
  ext_elev       = 5,
  use_bgc        = TRUE,
  aed            = aed_options(
    # Only oxygen and nutrient cycling (plus whatever they depend on)
    modules   = c("aed_sedflux", "aed_oxygen", "aed_nitrogen", "aed_phosphorus"),
    # Keep the sediment fluxes already in aed.nml rather than estimating them
    sed_zones = FALSE,
    # Don't derive the aed_totals variable lists
    totals    = FALSE
  )
)
```

By default
[`aed_options()`](https://limnotrack.com/reference/aed_options.md)
leaves every step on, so a plain `build_aeme(use_bgc = TRUE)` is
unchanged. Each step is also available on its own after the build:
[`set_glm_aed_models()`](https://limnotrack.com/reference/set_glm_aed_models.md),
[`set_aed_sed_const2d()`](https://limnotrack.com/reference/set_aed_sed_const2d.md)
and
[`set_aed_totals()`](https://limnotrack.com/reference/set_aed_totals.md).

------------------------------------------------------------------------

#### AED module dependency hierarchy

AED modules are not independent of one another — most reference state
variables that are only computed if another module is switched on. For
example, `aed_nitrogen` reduces ammonium using `OXY_oxy` as its oxidant,
so it requires `aed_oxygen` to also be active; if it is not, GLM aborts
at runtime with an “Undefined variable” error rather than silently
ignoring the missing link.

[`build_aeme()`](https://limnotrack.com/reference/build_aeme.md)
resolves this automatically: when you set `simulate = TRUE` for a
biogeochemical variable via `model_controls`, `initialise_aed()`
determines which module that variable belongs to, then walks the
dependency graph below to a fixed point, adding every module that
dependency chain requires — you never need to enable a module’s
prerequisites by hand. The one place this *isn’t* automatic is
[`set_glm_aed_models()`](https://limnotrack.com/reference/set_glm_aed_models.md)
(used above), which sets the active-module list exactly as given, so it
is up to you to include the full chain if you call it directly.
(`aed_options(modules = )` adds the prerequisites for you unless you set
`resolve_deps = FALSE`.)

The dependency graph, verified directly against the target-variable
links configured in AEME’s bundled `aed.nml` template:

    aed_sedflux
      └─ aed_oxygen           (sediment oxygen demand)
           ├─ aed_silica          (silica reactant = OXY_oxy)
           ├─ aed_nitrogen        (nitrification reactant = OXY_oxy; also needs aed_sedflux directly)
           ├─ aed_phosphorus      (P release reactant = OXY_oxy; also needs aed_sedflux directly)
           └─ aed_organic_matter  (mineralisation reactant = OXY_oxy)
                ├─ needs aed_nitrogen        (DON mineralisation product)
                ├─ needs aed_phosphorus      (DOP mineralisation product = PHS_frp)
                ├─ aed_zooplankton      (excretion/mortality targets)
                └─ aed_phytoplankton    (uptake/excretion/mortality targets;
                                          needs aed_oxygen, aed_nitrogen,
                                          aed_phosphorus, aed_silica, and
                                          aed_organic_matter directly)
                     └─ aed_totals      (TN/TP/TOC totals aggregate state
                                          variables owned by aed_nitrogen,
                                          aed_phosphorus, aed_organic_matter,
                                          and aed_phytoplankton)

    aed_macrophyte  (no enforced dependencies — no target-variable links in the
                     bundled template)

| Module | Required dependencies | Why |
|----|----|----|
| `aed_sedflux` | — | Sediment flux is the base of the chain; nothing else must be active first. |
| `aed_oxygen` | `aed_sedflux` | Sediment oxygen demand flux comes from `aed_sedflux`. |
| `aed_silica` | `aed_oxygen` | Silica reactant variable is `OXY_oxy`. |
| `aed_nitrogen` | `aed_oxygen`, `aed_sedflux` | Nitrification reactant is `OXY_oxy`; sediment NH₄/NO₃ fluxes come from `aed_sedflux`. |
| `aed_phosphorus` | `aed_oxygen`, `aed_sedflux` | Redox-sensitive P release reactant is `OXY_oxy`; sediment FRP flux comes from `aed_sedflux`. |
| `aed_organic_matter` | `aed_oxygen`, `aed_nitrogen`, `aed_phosphorus` | Mineralisation reactant is `OXY_oxy`; DON/DOP mineralisation products feed `aed_nitrogen`/`aed_phosphorus`. |
| `aed_phytoplankton` | `aed_oxygen`, `aed_nitrogen`, `aed_phosphorus`, `aed_silica`, `aed_organic_matter` | Growth/uptake/excretion/mortality target variables span all five modules (e.g. diatom silica uptake, nutrient uptake, DOM excretion). |
| `aed_zooplankton` | `aed_organic_matter` | Excretion and mortality products are organic-matter state variables. |
| `aed_totals` | `aed_nitrogen`, `aed_phosphorus`, `aed_organic_matter`, `aed_phytoplankton` | Its `TN_vars`/`TP_vars`/`TOC_vars` lists aggregate state variables owned by these four modules (e.g. `NIT_nit`, `PHS_frp`, `OGM_doc`, `PHY_green`). |
| `aed_macrophyte` | — | No target-variable links to other modules in the bundled template. |

AED module dependency requirements enforced by `initialise_aed()`.
{.table}

For example, requesting only `NIT_tn` (total nitrogen) in
`model_controls` looks like a single-variable request, but because it is
a diagnostic total computed by `aed_totals`, and `aed_totals` in turn
depends on nitrogen, phosphorus, organic matter, and phytoplankton,
[`build_aeme()`](https://limnotrack.com/reference/build_aeme.md)
activates the following module set automatically:

``` r

mc <- get_model_controls(use_bgc = TRUE) |>
  set_vars_sim(vars_sim = "NIT_tn")

aeme_nit_tn <- build_aeme(
  aeme           = aeme,
  model          = model,
  path           = path,
  model_controls = mc,
  ext_elev       = 5,
  use_bgc        = TRUE
)

config_files <- get_model_config_files(aeme)


nml <- read_nml(config_files$glm_aed["aed"])
get_nml_value(nml, "models")
#> [1] "aed_sedflux"        "aed_noncohesive"    "aed_oxygen"        
#> [4] "aed_silica"         "aed_nitrogen"       "aed_phosphorus"    
#> [7] "aed_organic_matter" "aed_phytoplankton"  "aed_totals"
```

------------------------------------------------------------------------

#### Sediment zones

One of the most important GLM-AED features for water-quality simulation
is **depth-varying sediment parameters**. GLM divides the lake bed into
*sediment zones*, each with its own temperature regime and (when using
`aed_sedflux` in `Constant2d` mode) its own sediment–water interface
fluxes of oxygen and nutrients.

Deeper zones typically accumulate more organic matter, experience longer
periods of anoxia, and therefore have higher sediment oxygen demand
(SOD) and nutrient release than shallow littoral zones.

##### Estimating zone boundaries from the hypsograph

[`estimate_sed_zones()`](https://limnotrack.com/reference/estimate_sed_zones.md)
automatically partitions the hypsograph into an appropriate number of
zones by detecting natural breakpoints in the depth–area relationship:

``` r

hypsograph   <- get_hypsograph(aeme)
zone_heights <- estimate_sed_zones(hypsograph)
zone_heights
#> [1]  3.07 22.00
```

The returned vector gives the *upper height* of each zone measured from
the lake bed (metres). The last value equals the maximum lake depth.

##### Generating GLM sediment parameters

[`glm_sed_params()`](https://limnotrack.com/reference/glm_sed_params.md)
builds a parameters data frame (compatible with the AEME `parameters`
slot) for the GLM sediment module:

``` r

sed_params <- glm_sed_params(
  n_zones            = length(zone_heights),
  zone_heights       = zone_heights,
  sed_temp_mean      = c(14, 16, 18)[seq_along(zone_heights)],
  sed_temp_amplitude = c(6, 4, 2)[seq_along(zone_heights)],
  sed_temp_peak_doy  = rep(30, length(zone_heights))
)
sed_params
#>      model     file                        name value    min    max group index
#> 1  glm_aed glm4.nml       sediment/benthic_mode  2.00  2.000  2.000  <NA>    NA
#> 2  glm_aed glm4.nml            sediment/n_zones  2.00  2.000  2.000  <NA>    NA
#> 3  glm_aed glm4.nml     sediment/sed_heat_Ksoil  0.01  0.005  0.015  <NA>     1
#> 4  glm_aed glm4.nml     sediment/sed_heat_Ksoil  0.01  0.005  0.015  <NA>     2
#> 5  glm_aed glm4.nml     sediment/sed_temp_depth  0.20  0.100  0.300  <NA>     1
#> 6  glm_aed glm4.nml     sediment/sed_temp_depth  0.20  0.100  0.300  <NA>     2
#> 7  glm_aed glm4.nml      sediment/sed_temp_mean 14.00  7.000 21.000  <NA>     1
#> 8  glm_aed glm4.nml      sediment/sed_temp_mean 16.00  8.000 24.000  <NA>     2
#> 9  glm_aed glm4.nml sediment/sed_temp_amplitude  6.00  3.000  9.000  <NA>     1
#> 10 glm_aed glm4.nml sediment/sed_temp_amplitude  4.00  2.000  6.000  <NA>     2
#> 11 glm_aed glm4.nml  sediment/sed_temp_peak_doy 30.00 15.000 45.000  <NA>     1
#> 12 glm_aed glm4.nml  sediment/sed_temp_peak_doy 30.00 15.000 45.000  <NA>     2
#> 13 glm_aed glm4.nml       sediment/zone_heights  3.07  1.535  4.605  <NA>     1
#> 14 glm_aed glm4.nml       sediment/zone_heights 22.00 11.000 33.000  <NA>     2
#> 15 glm_aed glm4.nml   sediment/sed_reflectivity  0.01  0.005  0.015  <NA>     1
#> 16 glm_aed glm4.nml   sediment/sed_reflectivity  0.01  0.005  0.015  <NA>     2
#> 17 glm_aed glm4.nml      sediment/sed_roughness  0.01  0.005  0.015  <NA>     1
#> 18 glm_aed glm4.nml      sediment/sed_roughness  0.01  0.005  0.015  <NA>     2
#>      module
#> 1  sediment
#> 2  sediment
#> 3  sediment
#> 4  sediment
#> 5  sediment
#> 6  sediment
#> 7  sediment
#> 8  sediment
#> 9  sediment
#> 10 sediment
#> 11 sediment
#> 12 sediment
#> 13 sediment
#> 14 sediment
#> 15 sediment
#> 16 sediment
#> 17 sediment
#> 18 sediment
```

Add these parameters to the `aeme` object so that they are applied
during the next
[`build_aeme()`](https://limnotrack.com/reference/build_aeme.md) call:

``` r

parameters(aeme) <- sed_params

# Rebuild to apply the new sediment zone parameters
aeme <- build_aeme(
  aeme           = aeme,
  model          = model,
  model_controls = model_controls,
  path           = path,
  ext_elev       = 5,
  use_bgc        = TRUE
)
```

##### Inspecting sediment zones in the built model

After building, you can retrieve the number of zones and their
parameters:

``` r

n_zones   <- get_glm_sed_zones(aeme = aeme, path = path)
sed_pars  <- get_glm_sed_params(aeme = aeme, path = path)
cat("Number of sediment zones:", n_zones, "\n")
#> Number of sediment zones: 2
sed_pars
#>      model     file                        name value   min   max index group
#> 1  glm_aed glm3.nml       sediment/benthic_mode  2.00  2.00  2.00    NA  <NA>
#> 2  glm_aed glm3.nml            sediment/n_zones  2.00  2.00  2.00    NA  <NA>
#> 3  glm_aed glm3.nml       sediment/zone_heights  3.07  3.07  3.07     1  <NA>
#> 4  glm_aed glm3.nml       sediment/zone_heights 22.00 22.00 22.00     2  <NA>
#> 5  glm_aed glm3.nml   sediment/sed_reflectivity  0.01  0.01  0.01     1  <NA>
#> 6  glm_aed glm3.nml   sediment/sed_reflectivity  0.01  0.01  0.01     2  <NA>
#> 7  glm_aed glm3.nml      sediment/sed_roughness  0.01  0.01  0.01     1  <NA>
#> 8  glm_aed glm3.nml      sediment/sed_roughness  0.01  0.01  0.01     2  <NA>
#> 9  glm_aed glm3.nml     sediment/sed_heat_model  2.00  2.00  2.00    NA  <NA>
#> 10 glm_aed glm3.nml       sediment/n_sed_layers  8.00  8.00  8.00    NA  <NA>
#> 11 glm_aed glm3.nml    sediment/sed_layer_depth  0.00  0.00  0.00     1  <NA>
#> 12 glm_aed glm3.nml    sediment/sed_layer_depth  0.02  0.02  0.02     2  <NA>
#> 13 glm_aed glm3.nml    sediment/sed_layer_depth  0.05  0.05  0.05     3  <NA>
#> 14 glm_aed glm3.nml    sediment/sed_layer_depth  0.10  0.10  0.10     4  <NA>
#> 15 glm_aed glm3.nml    sediment/sed_layer_depth  0.20  0.20  0.20     5  <NA>
#> 16 glm_aed glm3.nml    sediment/sed_layer_depth  0.40  0.40  0.40     6  <NA>
#> 17 glm_aed glm3.nml    sediment/sed_layer_depth  0.80  0.80  0.80     7  <NA>
#> 18 glm_aed glm3.nml    sediment/sed_layer_depth  1.50  1.50  1.50     8  <NA>
#> 19 glm_aed glm3.nml            sediment/sed_vwc  0.40  0.40  0.40    NA  <NA>
#> 20 glm_aed glm3.nml    sediment/sed_spinup_days 30.00 30.00 30.00    NA  <NA>
#> 21 glm_aed glm3.nml      sediment/sed_temp_mean 14.00 14.00 14.00     1  <NA>
#> 22 glm_aed glm3.nml      sediment/sed_temp_mean 16.00 16.00 16.00     2  <NA>
#> 23 glm_aed glm3.nml sediment/sed_temp_amplitude  6.00  6.00  6.00     1  <NA>
#> 24 glm_aed glm3.nml sediment/sed_temp_amplitude  4.00  4.00  4.00     2  <NA>
#> 25 glm_aed glm3.nml  sediment/sed_temp_peak_doy 30.00 30.00 30.00     1  <NA>
#> 26 glm_aed glm3.nml  sediment/sed_temp_peak_doy 30.00 30.00 30.00     2  <NA>
#> 27 glm_aed glm3.nml      sediment/sed_temp_deep 10.00 10.00 10.00     1  <NA>
#> 28 glm_aed glm3.nml      sediment/sed_temp_deep 10.00 10.00 10.00     2  <NA>
#> 29 glm_aed glm3.nml     sediment/sed_heat_Ksoil  0.01  0.01  0.01     1  <NA>
#> 30 glm_aed glm3.nml     sediment/sed_heat_Ksoil  0.01  0.01  0.01     2  <NA>
#> 31 glm_aed glm3.nml     sediment/sed_temp_depth  0.20  0.20  0.20     1  <NA>
#> 32 glm_aed glm3.nml     sediment/sed_temp_depth  0.20  0.20  0.20     2  <NA>
```

##### Estimating depth-varying sediment fluxes

[`estimate_zone_fluxes()`](https://limnotrack.com/reference/estimate_zone_fluxes.md)
scales literature-baseline sediment fluxes to each zone according to its
mean depth and bed-area fraction (Tier 1). When observed near-bed
concentrations are available in the `aeme` object, an optional Tier 2
adjustment refines the inter-zone ratios using summer near-bottom data:

``` r

fluxes <- estimate_zone_fluxes(
  aeme      = aeme,
  path      = path,
  baseline  = c(
    fsed_oxy = -25,   # mmol O2/m2/d  (negative = into sediment)
    fsed_amm =   2,   # mmol N/m2/d
    fsed_nit =   0.2, # mmol N/m2/d
    fsed_frp =   0.05 # mmol P/m2/d
  ),
  verbose   = TRUE
)
```

The zone summary shows each zone’s depth range, bed area, and estimated
fluxes:

``` r

fluxes$zone_summary
#>   zone height_lower_m height_upper_m depth_upper_m depth_lower_m mean_depth_m
#> 1    1           0.00           3.07            10         13.07        11.54
#> 2    2           3.07          22.00             0         10.00         5.00
#>   area_m2 area_frac fsed_oxy fsed_amm fsed_nit fsed_frp
#> 1   43957     0.289    -38.8    5.698     -0.4   0.1072
#> 2  108386     0.711    -19.4    0.500      0.1   0.0268
```

##### Applying sediment fluxes to the AED configuration

[`set_aed_sed_const2d()`](https://limnotrack.com/reference/set_aed_sed_const2d.md)
writes the zone-specific fluxes directly into the `aed/aed.nml` file and
updates the `aeme` object:

``` r

aeme <- set_aed_sed_const2d(
  aeme     = aeme,
  path     = path,
  baseline = c(
    fsed_oxy = -25,
    fsed_amm =  2,
    fsed_nit =  0.2,
    fsed_frp =  0.05
  )
)
```

After calling
[`set_aed_sed_const2d()`](https://limnotrack.com/reference/set_aed_sed_const2d.md),
you can confirm the written parameters:

``` r

get_aed_sed_const2d_param(aeme = aeme, path = path) |>
  dplyr::select(name, value, index) |>
  head(20)
#> # A tibble: 11 × 3
#>    name                          value index
#>    <chr>                         <dbl> <dbl>
#>  1 aed_sed_const2d/active_zones   1        1
#>  2 aed_sed_const2d/active_zones   2        2
#>  3 aed_sed_const2d/fsed_amm       2        1
#>  4 aed_sed_const2d/fsed_amm       2        2
#>  5 aed_sed_const2d/fsed_frp       0.05     1
#>  6 aed_sed_const2d/fsed_frp       0.05     2
#>  7 aed_sed_const2d/fsed_nit      -0.2      1
#>  8 aed_sed_const2d/fsed_nit      -0.2      2
#>  9 aed_sed_const2d/fsed_oxy     -25        1
#> 10 aed_sed_const2d/fsed_oxy     -25        2
#> 11 aed_sed_const2d/n_zones        2       NA
```

------------------------------------------------------------------------

#### Visualising the model configuration

[`plot_glm_config()`](https://limnotrack.com/reference/plot_glm_config.md)
generates an interactive HTML visualisation of the complete GLM-AED
setup, including the hypsograph, sediment zones, inflow/outflow
positions, active AED modules, and key parameter values. If called
inside an RStudio session, the output opens in the Viewer pane;
otherwise it is saved to a temporary HTML file.

``` r

config_html <- plot_glm_config(aeme = aeme)
```

This html widget provides a comprehensive overview of the model
configuration, making it easy to verify that the setup matches your
intentions and to identify any potential issues before running the
model.

You can view the HTML widget directly in RStudio’s Viewer pane or open
it in your web browser.

Show parameter labels

## 

### Hypsograph & sediment zones

### AED biogeochemical modules

------------------------------------------------------------------------

### Running GLM-AED

``` r

aeme <- run_aeme(aeme = aeme)
```

------------------------------------------------------------------------

### Visualising model output

#### Temperature and stratification

[`plot_output()`](https://limnotrack.com/reference/plot_output.md)
produces a filled contour plot for depth-varying variables
(e.g. temperature) and a time-series line plot for scalar variables:

``` r

plot_output(aeme = aeme, var_sim = "HYD_temp")
```

![Simulated water temperature (°C) at each model layer over
time.](glm-aed_files/figure-html/plot-temp-1.png)

Simulated water temperature (°C) at each model layer over time.

#### Water quality variables

Any variable that was listed in `vars_sim` and is present in the output
can be plotted the same way:

``` r

plot_output(aeme = aeme, var_sim = "CHM_oxy")
```

![Simulated dissolved oxygen (mmol O2 m⁻³)
profiles.](glm-aed_files/figure-html/plot-oxy-1.png)

Simulated dissolved oxygen (mmol O2 m⁻³) profiles.

``` r

plot_output(aeme = aeme, var_sim = "PHY_tchla")
```

![Simulated total chlorophyll-a (µg L⁻¹) time
series.](glm-aed_files/figure-html/plot-tchla-1.png)

Simulated total chlorophyll-a (µg L⁻¹) time series.

------------------------------------------------------------------------

### Assessing model performance

When observations are stored in the `aeme` object,
[`assess_model()`](https://limnotrack.com/reference/assess_aeme.md)
computes a suite of skill metrics (RMSE, NSE, bias, Pearson *r*, etc.)
for each simulated variable:

``` r

skill <- assess_aeme(aeme = aeme, model = model)
skill
#>      Model           name_text   var_aeme      bias      mae     rmse   nmae
#> 1  GLM-AED Dissolved organic C    CAR_doc -2.70e+00 2.70e+00 2.75e+00 0.9940
#> 2  GLM-AED    Dissolved oxygen    CHM_oxy  1.13e+00 1.26e+00 1.93e+00 0.1830
#> 3  GLM-AED      Oxycline depth CHM_oxycln  1.89e+00 1.89e+00 2.26e+00 0.2100
#> 4  GLM-AED            Salinity   CHM_salt -1.17e-01 1.17e-01 1.17e-01 1.0000
#> 5  GLM-AED          Stratified  HYD_strat  0.00e+00 2.00e-01 4.47e-01 0.2860
#> 6  GLM-AED   Water temperature   HYD_temp -2.30e-01 8.20e-01 1.13e+00 0.0455
#> 7  GLM-AED   Thermocline depth HYD_thmcln -2.54e+00 3.21e+00 4.60e+00 0.3340
#> 8  GLM-AED         Water level LKE_lvlwtr -3.60e-02 1.29e-01 1.67e-01 0.0055
#> 9  GLM-AED              Volume    LKE_vol -5.31e+03 1.94e+04 2.49e+04 0.0177
#> 10 GLM-AED Ammoniacal nitrogen    NIT_amm  3.16e-03 1.23e-02 2.66e-02 1.0400
#> 11 GLM-AED             Nitrate    NIT_nit  2.98e-03 3.24e-03 5.07e-03 2.0300
#> 12 GLM-AED      Total nitrogen     NIT_tn -1.43e-01 1.43e-01 1.49e-01 0.7560
#> 13 GLM-AED           Phosphate    PHS_frp -1.23e-03 1.46e-03 1.64e-03 0.8110
#> 14 GLM-AED    Total phosphorus     PHS_tp -8.22e-03 8.30e-03 1.01e-02 0.7350
#> 15 GLM-AED       Cyanobacteria  PHY_cyano -2.69e-02 3.13e-02 6.25e-02 0.8360
#> 16 GLM-AED Total chlorophyll a  PHY_tchla  2.12e-01 6.50e+00 7.79e+00 0.8990
#>          nse     kge     d2       r      rs        B   n obs_na sim_na
#> 1   -33.0000 -1.0400 0.2120 -0.4920 -0.3940 0.006910  10      0      0
#> 2     0.6340  0.6790 0.8810  0.8810  0.9140 0.568000 125      0      0
#> 3    -0.3510  0.5760 0.7090  0.7700  0.8260 0.252000  24      0      0
#> 4  -329.0000      NA 0.0827      NA      NA 0.000000 125      0      0
#> 5     0.0476  0.5240 0.7520  0.5240  0.5240 0.141000  10      0      0
#> 6     0.8690  0.9350 0.9670  0.9390  0.9240 0.779000 125      0      0
#> 7    -1.4300  0.2670 0.6240  0.4910  0.4160 0.070300  10      0      0
#> 8    -7.1000 -0.8960 0.3260  0.0434 -0.0732 0.000206   8      0      0
#> 9    -6.9900 -0.8790 0.3260  0.0394 -0.0732 0.000172   8      0      0
#> 10   -0.9150  0.2090 0.6060  0.4690  0.5790 0.075500  20      0      0
#> 11   -5.6900 -1.2900 0.3050  0.2290  0.4290 0.006830  20      0      0
#> 12  -29.4000 -0.1720 0.2040  0.1680 -0.2290 0.000902  20      0      0
#> 13   -2.5400  0.0355 0.4990  0.3360  0.4070 0.024900  20      0      0
#> 14   -2.6800 -0.4910 0.3760 -0.1290 -0.3020 0.003590  20      0      0
#> 15   -0.3080 -0.6500 0.3600 -0.2010 -0.1310 0.017600  10      0      0
#> 16   -4.6100 -0.5390 0.3220 -0.2240 -0.3210 0.007590  10      0      0
```

------------------------------------------------------------------------

### GLM-AED diagnostics

#### Comprehensive diagnostic report

[`run_glm_aed_diagnostics()`](https://limnotrack.com/reference/run_glm_aed_diagnostics.md)
reads the model output and produces a structured diagnostic report — a
summary table and a set of grouped plots — that helps you quickly
identify unrealistic values or mass-balance issues:

``` r

diag <- run_glm_aed_diagnostics(
  aeme        = aeme,
  plot        = TRUE,
  print_table = TRUE
)
```

![](glm-aed_files/figure-html/run-diagnostics-1.png)![](glm-aed_files/figure-html/run-diagnostics-2.png)![](glm-aed_files/figure-html/run-diagnostics-3.png)![](glm-aed_files/figure-html/run-diagnostics-4.png)![](glm-aed_files/figure-html/run-diagnostics-5.png)

The function returns a list with three components:

- `$summary` — a data frame with min, median, mean, max, and a `flag`
  column (`"ok"` or a warning string) for each variable.
- `$plots` — a named list of `patchwork` plot objects grouped by
  biogeochemical element (`O`, `N`, `P`, `Phy`, `Sed`).
- `$data` — the tidy data frame used to produce the plots.

You can filter the diagnostics to a specific element or type:

``` r

# Nitrogen state variables only
diag_N <- run_glm_aed_diagnostics(
  aeme        = aeme,
  groups      = "N",
  plot        = TRUE,
  print_table = FALSE
)

# Process-rate variables only
diag_proc <- run_glm_aed_diagnostics(
  aeme        = aeme,
  groups      = "process",
  plot        = FALSE,
  print_table = TRUE
)
```

#### Oxygen diagnostic page

[`plot_glm_diagnostics()`](https://limnotrack.com/reference/plot_glm_diagnostics.md)
provides a focused four-page diagnostic panel specifically designed to
debug anomalous dissolved oxygen behaviour — a common challenge when
coupling hydrodynamics with biogeochemistry:

``` r

pages <- plot_glm_diagnostics(aeme = aeme)

# Page 1: oxygen state and key physical drivers
print(pages$oxy)
```

![](glm-aed_files/figure-html/plot-diag-1.png)

``` r

# Page 2: mixing and physical structure
print(pages$physical)
```

![](glm-aed_files/figure-html/plot-diag-mixing-1.png)

``` r

# Page 3: biological oxygen demand
print(pages$bod)
```

![](glm-aed_files/figure-html/plot-diag-bod-1.png)

``` r

# Page 4: sediment–water interface fluxes
print(pages$sediment)
```

![](glm-aed_files/figure-html/plot-diag-sed-1.png)

------------------------------------------------------------------------

### Working with the GLM configuration directly

The raw GLM hydrodynamic nml (`glm3.nml` or `glm4.nml`, resolved via
[`find_glm_nml()`](https://limnotrack.com/reference/find_glm_nml.md))
and `aed/aed.nml` file can be read, modified, and written using the NML
helpers bundled with AEME:

``` r

# Retrieve the parsed configuration
cfg <- read_model_config(model = "glm_aed",
                         lake_dir = get_lake_dir(aeme))

# Access GLM hydrodynamic section
glm_nml <- cfg$hydrodynamic
glm_nml$morphometry$lake_name

# Access AED biogeochemistry section
aed_nml <- cfg$bgc$aed
aed_nml$aed_nitrogen$rnitrif   # nitrification rate
```

For quick edits to a single parameter without loading the whole config,
[`get_glm_param()`](https://limnotrack.com/reference/get_glm_param.md)/[`set_glm_param()`](https://limnotrack.com/reference/set_glm_param.md)
work directly against the nml file on disk (see
`vignette("testing-parameters")`):

``` r

path_glm <- file.path(get_lake_dir(aeme), "glm_aed")
get_glm_param(path_glm, "Kw")
set_glm_param(path_glm, Kw = 0.5, coef_mix_hyp = 0.3)
```

#### Retrieving parameters by module

[`get_aeme_parameters()`](https://limnotrack.com/reference/get_aeme_parameters.md)
provides a convenient way to query the AEME parameter library for all
GLM-AED parameters belonging to a particular module:

``` r

# All GLM-AED light parameters
light_params <- get_aeme_parameters(model = "glm_aed",
                                    module = "light")
light_params[, c("name", "value", "min", "max")]
#> # A tibble: 6 × 4
#>   name               value   min    max
#>   <chr>              <dbl> <dbl>  <dbl>
#> 1 light/light_mode    0    0      0    
#> 2 light/n_bands       4    2      6    
#> 3 light/light_extc    1    0.5    1.5  
#> 4 light/energy_frac   0.51 0.255  0.765
#> 5 light/Benthic_Imin 10    5     15    
#> 6 light/Kw            0.2  0.1    0.3
```

``` r

# AED nitrogen module parameters
n_params <- get_aeme_parameters(model = "glm_aed",
                                module = "nitrogen")
n_params[, c("name", "value", "min", "max")]
#> # A tibble: 62 × 4
#>    name                          value     min      max
#>    <chr>                         <dbl>   <dbl>    <dbl>
#>  1 aed2_nitrogen/theta_sed_amm   1.08   0.54     1.62  
#>  2 aed2_nitrogen/Fsed_amm       30     15       45     
#>  3 aed2_nitrogen/Ksed_amm       31.2   15.6     46.9   
#>  4 aed2_nitrogen/Fsed_n2o        0      0        0     
#>  5 aed2_nitrogen/Ksed_n2o      100     50      150     
#>  6 aed2_nitrogen/theta_sed_nit   1.08   0.54     1.62  
#>  7 aed2_nitrogen/Fsed_nit        5.2    2.6      7.8   
#>  8 aed2_nitrogen/Ksed_nit      100     50      150     
#>  9 aed2_nitrogen/Kpart_ammox     1      0.5      1.5   
#> 10 aed2_nitrogen/Ranammox        0.001  0.0005   0.0015
#> # ℹ 52 more rows
```

------------------------------------------------------------------------

### Summary

This vignette demonstrated the key GLM-AED-specific features available
in AEME:

1.  **Model description** — GLM provides variable-layer 1-D
    hydrodynamics; AED supplies modular biogeochemistry ranging from
    simple oxygen dynamics through to full nutrient–phytoplankton
    food-web simulation.

2.  **Module selection** —
    [`set_glm_aed_models()`](https://limnotrack.com/reference/set_glm_aed_models.md)
    lets you enable or disable individual AED modules to match the
    complexity and data requirements of your application.

3.  **Sediment zones** — The
    [`estimate_sed_zones()`](https://limnotrack.com/reference/estimate_sed_zones.md)
    →
    [`glm_sed_params()`](https://limnotrack.com/reference/glm_sed_params.md)
    →
    [`set_aed_sed_const2d()`](https://limnotrack.com/reference/set_aed_sed_const2d.md)
    workflow automatically derives depth-varying sediment parameters
    from the lake hypsograph and optional near-bed observations.

4.  **Configuration visualisation** —
    [`plot_glm_config()`](https://limnotrack.com/reference/plot_glm_config.md)
    provides an interactive overview of the model setup.

5.  **Diagnostics** —
    [`run_glm_aed_diagnostics()`](https://limnotrack.com/reference/run_glm_aed_diagnostics.md)
    and
    [`plot_glm_diagnostics()`](https://limnotrack.com/reference/plot_glm_diagnostics.md)
    offer structured, automated checks for common biogeochemical issues
    (unrealistic concentrations, oxygen anomalies, excessive or
    negligible fluxes).

6.  **Parameter access** —
    [`get_aeme_parameters()`](https://limnotrack.com/reference/get_aeme_parameters.md)
    makes it easy to explore and modify individual model parameters for
    manual tuning or automated calibration (see the
    [aemetools](https://github.com/limnotrack/aemetools) package).
