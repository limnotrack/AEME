# Parameter library for the GLM-AED model.

This dataset contains most parameters within the glm3.nml and the
aed.nml file. This includes 156 GLM parameters across 14 namelist blocks
(e.g., `&glm_setup`, `&mixing`, `&morphometry`, `&meteorology`,
`&inflow`, `&outflow`) and 125 AED parameters across 7 modules (e.g.,
`aed_oxygen`, `aed_nitrogen`, `aed_phosphorus`, `aed_silica`,
`aed_phytoplankton`, `aed_sedflux`, `aed_seddiagenesis`), for a combined
total of 281 rows.

## Usage

``` r
glm_aed_parameter_library
```

## Format

A data frame with 281 rows and 9 variables:

- module:

  Character. The configuration block (GLM) or module (AED) the parameter
  belongs to. GLM modules correspond to Fortran namelist blocks in
  glm3.nml (e.g., `"glm_setup"`, `"mixing"`, `"morphometry"`, `"time"`,
  `"output"`, `"init_profiles"`, `"light"`, `"bird_model"`,
  `"sediment"`, `"snowice"`, `"meteorology"`, `"inflow"`, `"outflow"`,
  `"wq_setup"`). AED modules correspond to namelist blocks in aed.nml
  (e.g., `"aed_oxygen"`, `"aed_nitrogen"`, `"aed_phosphorus"`,
  `"aed_silica"`, `"aed_phytoplankton"`, `"aed_sedflux"`,
  `"aed_seddiagenesis"`).

- group:

  Character. An optional sub-category grouping related parameters by
  process or function within a module. For example, `"nitrification"`,
  `"denitrification"`, and `"sediment_flux"` within the `aed_nitrogen`
  module; or `"radiation"`, `"wind"`, and `"rainfall"` within the
  `meteorology` module. Empty string (`""`) where no sub-grouping
  applies.

- parameter:

  Character. The parameter name as used in the configuration file (e.g.,
  `"max_layers"`, `"Fsed_oxy"`, `"R_nitrif"`).

- label:

  Character. A short, human-readable label for the parameter (e.g.,
  `"Layers"`, `"Sediment O2 flux"`, `"Nitrification rate"`).

- symbol:

  Character. The mathematical symbol used in the model documentation and
  equations, rendered as plain text (e.g., `"N_MAX"`, `"F_sed^oxy"`,
  `"theta_nitrif"`). Empty string where no symbol is defined.

- description:

  Character. A description of the parameter, its role in the model, and
  any relevant context.

- units:

  Character. The units of the parameter. Uses `"-"` for dimensionless
  quantities, `"string"` for text parameters, and `"boolean"` for
  logical switches. Physical units follow standard abbreviations (e.g.,
  `"m"`, `"mmol O2/m3"`, `"mmol N/m2/d"`, `"/d"`).

- default:

  Character. The default value of the parameter as documented, or an
  empty string (`""`) where no default is specified in the source
  documentation.

- source:

  Character. URL of the documentation page from which the parameter
  information was extracted. GLM parameters are sourced from
  <https://aquatic.science.uwa.edu.au/research/models/GLM/configuration.html>
  and AED parameters from individual chapter pages of the AED Science
  Manual at <https://aquaticecodynamics.github.io/aed-science/>.

## Source

GLM configuration reference:
<https://aquatic.science.uwa.edu.au/research/models/GLM/configuration.html>

AED Science Manual: <https://aquaticecodynamics.github.io/aed-science/>

Hipsey, M.R., Bruce, L.C., Boon, C., Busch, B., Carey, C.C., Hamilton,
D.P., Hanson, P.C., Read, J.S., de Sousa, E., Weber, M. and Winslow,
L.A. (2019). A General Lake Model (GLM 3.0) for linking with
high-frequency sensor data from the Global Lake Ecological Observatory
Network (GLEON). *Geoscientific Model Development*, 12, 473–523.
[doi:10.5194/gmd-12-473-2019](https://doi.org/10.5194/gmd-12-473-2019)

Hipsey, M.R. (Ed.) (2022). Modelling Aquatic Eco-Dynamics: Overview of
the AED modular simulation platform. Zenodo.
[doi:10.5281/zenodo.6516222](https://doi.org/10.5281/zenodo.6516222)

## Details

Parameters are organised by `module` (the namelist block or AED module
they belong to) and optionally by `group` (a sub-category describing the
process context, such as `"nitrification"`, `"sediment_flux"`, or
`"radiation"`).

The GLM (General Lake Model) parameters configure the hydrodynamic
model, including layer structure, mixing, morphometry, meteorological
forcing, boundary conditions (inflows and outflows), light penetration,
ice/snow dynamics, and sediment heat exchange.

The AED (Aquatic EcoDynamics) parameters configure the water quality and
biogeochemical modules that couple to GLM. The modules included in this
dataset cover dissolved oxygen, inorganic nitrogen (ammonium, nitrate,
nitrification, denitrification), inorganic phosphorus (phosphate,
adsorption), silica, phytoplankton (growth, light, nutrient uptake,
respiration, mortality, settling), and sediment biogeochemistry (static
sediment fluxes via `aed_sedflux` and dynamic diagenesis via
`aed_seddiagenesis`).

Note that `aed_phytoplankton` parameters listed under groups such as
`"growth"`, `"light"`, `"nitrogen"`, `"phosphorus"`, and `"silica"` are
per-phytoplankton-group parameters typically specified in a separate
`phyto_data` namelist file rather than directly in `aed.nml`.

## Examples

``` r
# Load the parameter library
data(glm_aed_parameter_library)

# View the structure
str(glm_aed_parameter_library)
#> spc_tbl_ [281 × 9] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
#>  $ module     : chr [1:281] "glm_setup" "glm_setup" "glm_setup" "glm_setup" ...
#>  $ group      : chr [1:281] NA NA NA NA ...
#>  $ parameter  : chr [1:281] "sim_name" "max_layers" "min_layer_vol" "min_layer_thick" ...
#>  $ label      : chr [1:281] "Run name" "Layers" "Min layer volume" "Min layer thickness" ...
#>  $ symbol     : chr [1:281] NA "N_MAX" "Delta_V_min" "Delta_z_min" ...
#>  $ description: chr [1:281] "Title of simulation" "Maximum number of layers" "Minimum layer volume" "Minimum thickness of a layer" ...
#>  $ units      : chr [1:281] "string" "-" "m3" "m" ...
#>  $ default    : chr [1:281] "lake" "500" NA NA ...
#>  $ source     : chr [1:281] "https://aquatic.science.uwa.edu.au/research/models/GLM/configuration.html" "https://aquatic.science.uwa.edu.au/research/models/GLM/configuration.html" "https://aquatic.science.uwa.edu.au/research/models/GLM/configuration.html" "https://aquatic.science.uwa.edu.au/research/models/GLM/configuration.html" ...
#>  - attr(*, "spec")=
#>   .. cols(
#>   ..   module = col_character(),
#>   ..   group = col_character(),
#>   ..   parameter = col_character(),
#>   ..   label = col_character(),
#>   ..   symbol = col_character(),
#>   ..   description = col_character(),
#>   ..   units = col_character(),
#>   ..   default = col_character(),
#>   ..   source = col_character()
#>   .. )
#>  - attr(*, "problems")=<externalptr> 

# List all unique modules
unique(glm_aed_parameter_library$module)
#>  [1] "glm_setup"         "mixing"            "wq_setup"         
#>  [4] "morphometry"       "time"              "output"           
#>  [7] "init_profiles"     "light"             "bird_model"       
#> [10] "sediment"          "snowice"           "meteorology"      
#> [13] "inflow"            "outflow"           "aed_oxygen"       
#> [16] "aed_nitrogen"      "aed_phosphorus"    "aed_silica"       
#> [19] "aed_phytoplankton" "aed_sedflux"       "aed_seddiagenesis"

# Filter to a specific module
glm_aed_parameter_library[glm_aed_parameter_library$module == "aed_oxygen", ]
#> # A tibble: 9 × 9
#>   module     group       parameter label symbol description units default source
#>   <chr>      <chr>       <chr>     <chr> <chr>  <chr>       <chr> <chr>   <chr> 
#> 1 aed_oxygen initialisa… oxy_init… Init… O2|t=0 Initial di… mmol… NA      https…
#> 2 aed_oxygen initialisa… oxy_min   Min … O2_min Minimum di… mmol… 0       https…
#> 3 aed_oxygen initialisa… oxy_max   Max … O2_max Maximum di… mmol… 600     https…
#> 4 aed_oxygen sediment_f… Fsed_oxy  Sedi… F_sed… Sediment O… mmol… NA      https…
#> 5 aed_oxygen sediment_f… Ksed_oxy  Half… K_sod… Half-satur… mmol… NA      https…
#> 6 aed_oxygen sediment_f… theta_se… Thet… theta… Arrhenius … -     NA      https…
#> 7 aed_oxygen sediment_f… Fsed_oxy… Sed … NA     Variable n… stri… SDF_Fs… https…
#> 8 aed_oxygen atmospheri… oxy_pist… Pist… Theta… Selection … -     NA      https…
#> 9 aed_oxygen atmospheri… altitude  Alti… H      Altitude o… m     0       https…

# Get all phytoplankton growth parameters
glm_aed_parameter_library[glm_aed_parameter_library$module == "aed_phytoplankton" &
                          glm_aed_parameter_library$group == "growth", ]
#> # A tibble: 6 × 9
#>   module           group parameter label symbol description units default source
#>   <chr>            <chr> <chr>     <chr> <chr>  <chr>       <chr> <chr>   <chr> 
#> 1 aed_phytoplankt… grow… R_growth  Max … R_gro… Maximum ph… /d    NA      https…
#> 2 aed_phytoplankt… grow… fT_Method Temp… Theta… Method for… -     1       https…
#> 3 aed_phytoplankt… grow… theta_gr… Thet… theta… Arrhenius … -     1.06    https…
#> 4 aed_phytoplankt… grow… T_std     Stan… T_std  Standard t… degC  20      https…
#> 5 aed_phytoplankt… grow… T_opt     Opti… T_opt  Optimum te… degC  25      https…
#> 6 aed_phytoplankt… grow… T_max     Maxi… T_max  Maximum te… degC  35      https…

# Count parameters per module
table(glm_aed_parameter_library$module)
#> 
#>      aed_nitrogen        aed_oxygen    aed_phosphorus aed_phytoplankton 
#>                27                 9                15                42 
#> aed_seddiagenesis       aed_sedflux        aed_silica        bird_model 
#>                18                 7                 7                 6 
#>         glm_setup            inflow     init_profiles             light 
#>                 7                12                 8                 7 
#>       meteorology            mixing       morphometry           outflow 
#>                28                 9                10                28 
#>            output          sediment           snowice              time 
#>                15                10                 3                 6 
#>          wq_setup 
#>                 7 
```
