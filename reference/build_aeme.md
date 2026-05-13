# Build model configuration directories

Configure an ensemble of lake model simulations from basic set of
inputs.

## Usage

``` r
build_aeme(
  aeme = NULL,
  model = c("dy_cd", "glm_aed", "gotm_wet"),
  path = ".",
  model_controls = NULL,
  inf_factor = NULL,
  outf_factor = NULL,
  ext_elev = 0,
  use_bgc = FALSE,
  calc_wbal = TRUE,
  wb_method = 2,
  calc_wlev = TRUE,
  use_aeme = FALSE,
  coeffs = NULL,
  hum_type = 3,
  est_swr_hr = TRUE,
  config = NULL
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

- inf_factor:

  vector; containing numeric factor to multiple the inflows. Needs to be
  named according to the model.

- outf_factor:

  vector; containing numeric factor to multiple the outflows. Needs to
  be named according to the model.

- ext_elev:

  numeric; metres to extend the hypograph by.

- use_bgc:

  logical; switch to use the biogeochemical model.

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

- config:

  list; loaded via \`config \<- yaml::read_yaml("aeme.yaml")\`

## Value

aeme object

## Examples

``` r
# Read in example AEME object and build model configuration files for GLM-AED
aeme_dir <- system.file("extdata/lake/", package = "AEME")
path <- "aeme" # subdirectory where model configuration files will be written
aeme <- yaml_to_aeme(path = aeme_dir, "aeme.yaml")
model_controls <- get_model_controls()
model <- c("glm_aed")
aeme <- aeme |> 
  build_aeme(path = path, model = model, model_controls = model_controls,
           ext_elev = 5)
#> ✔ Created missing directory: D:\a\AEME\AEME\docs\reference\aeme
#> ℹ All columns already match AEME standard variable names, skipping name
#>   guessing.
#> ℹ All columns already match AEME standard inflow variable names, skipping name
#>   guessing.
#> Warning: ! `SIL_rsi`: SIL_rsi is constant across all rows — this may be a placeholder
#>   value.
#> ℹ Check raw data or unit conversion for this variable.
#> ℹ Using observed water level.
#> ! Missing values in observed water level.
#> ℹ Correcting water balance using estimated outflows (method = 2).
#> ℹ Calculating lake level using lake depth and a sinisoidal function.
#> ℹ Building GLM-AED for lake wainamu
#> ℹ Copied in GLM nml file
#> ℹ Copied in AED nml file and supporting files
#> ✔ GLM nml validation completed - no issues detected.

# Switch on biogeochemistry and use default model controls
aeme <- aeme |>
  build_aeme(path = path, model = model, model_controls = model_controls, 
              ext_elev = 5, use_bgc = TRUE)
#> ℹ All columns already match AEME standard variable names, skipping name
#>   guessing.
#> ℹ All columns already match AEME standard inflow variable names, skipping name
#>   guessing.
#> Warning: ! `SIL_rsi`: SIL_rsi is constant across all rows — this may be a placeholder
#>   value.
#> ℹ Check raw data or unit conversion for this variable.
#> Warning: ! 1 missing state variable in `FWMT`:
#> ✖ `ZOO_zoo1 `
#> ℹ Filled 1 missing variable with default value from `model_controls`.
#> ℹ Using observed water level.
#> ! Missing values in observed water level.
#> ℹ Correcting water balance using estimated outflows (method = 2).
#> ℹ Calculating lake level using lake depth and a sinisoidal function.
#> ℹ Building GLM-AED for lake wainamu
#> ℹ 15 replaced with 41.6285
#> ℹ 15 replaced with 16.6514
#> ℹ 225 replaced with 312.5
#> ℹ 2.25 replaced with 1.4279
#> ℹ 21 replaced with 21.4183
#> ℹ 6.96 replaced with 1.0709
#> ℹ 19.8 replaced with 7.1394
#> ℹ 0.008 replaced with 0.3229
#> ℹ 0.05 replaced with 0.3229
#> ℹ 0.05 replaced with 0.3229
#> ℹ PHY_cyano 10 replaced with 0.24022
#> ℹ PHY_diatom 8.4 replaced with 0.300275
#> ℹ PHY_green 0.04 replaced with 0.300275
#> ℹ 100 replaced with 1
#> ℹ Using default zooplankton initialisation
#> ✔ Updated GLM-AED models from: aed_sedflux, aed_oxygen, aed_silica,
#>   aed_nitrogen, aed_phosphorus, aed_organic_matter, aed_phytoplankton,
#>   aed_zooplankton, aed_macrophyte, aed_totals to: aed_sedflux, aed_oxygen,
#>   aed_silica, aed_nitrogen, aed_phosphorus, aed_organic_matter,
#>   aed_phytoplankton, aed_totals
#> ℹ Setting up AED aed_sed_const2d sediment zones: 2
#> 
#> Tier 2: zone-median summer concentrations used for adjustment:
#>         oxy   amm   nit   frp
#> Zone1 0.075 0.078 0.010 0.004
#> Zone2 7.160 0.005 0.001 0.002
#> Tier 2 adjustments applied: fsed_amm (2 zones, direct NH4); fsed_frp (2 zones, direct FRP)
#> 
#> === Sediment zone flux estimates (obs_adjusted) ===
#> n_zones: 2 | max lake depth: 13.07 m | ref_depth: 5 m
#> 
#>  zone height_lower_m height_upper_m depth_upper_m depth_lower_m mean_depth_m
#>     1           0.00           3.07            10          13.1         11.5
#>     2           3.07          19.00             0          10.0          5.0
#>  area_m2 area_frac fsed_oxy fsed_amm fsed_nit fsed_frp
#>    43957     0.289    -38.8    5.835     -0.4   0.1035
#>   108386     0.711    -19.4    0.512      0.1   0.0259
#> 
#> Lake-wide area-weighted average fluxes (for sanity check):
#>     oxy     amm     nit     frp 
#> -25.007   2.050  -0.044   0.048 
#> ✔ GLM nml validation completed - no issues detected.
```
