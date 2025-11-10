# Build model configuration directories

Configure an ensemble of lake model simulations from basic set of
inputs.

## Usage

``` r
build_aeme(
  aeme = NULL,
  config = NULL,
  model = c("dy_cd", "glm_aed", "gotm_wet"),
  model_controls = NULL,
  inf_factor = c(glm_aed = 1, dy_cd = 1, gotm_wet = 1),
  outf_factor = c(glm_aed = 1, dy_cd = 1, gotm_wet = 1),
  ext_elev = 0,
  use_bgc = FALSE,
  calc_wbal = TRUE,
  wb_method = 2,
  calc_wlev = TRUE,
  use_aeme = FALSE,
  coeffs = NULL,
  hum_type = 3,
  est_swr_hr = TRUE,
  path = "."
)
```

## Arguments

- aeme:

  aeme; object.

- config:

  list; loaded via \`config \<- yaml::read_yaml("aeme.yaml")\`

- model:

  vector; of models to be used. Can be \`dy_cd\`, \`glm_aed\`,
  \`gotm_wet\`.

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
  inflows or outflows) or 2 (outflows calculated) or 3 (inflows and
  outflows calculated). Default = 2

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

- path:

  filepath; where input files are located relative to the current
  working directory.

## Value

builds the model ensemble configuration.

aeme object

## Examples

``` r
tmpdir <- tempdir()
aeme_dir <- system.file("extdata/lake/", package = "AEME")
# Copy files from package into tempdir
file.copy(aeme_dir, tmpdir, recursive = TRUE)
#> [1] TRUE
path <- file.path(tmpdir, "lake")
aeme <- yaml_to_aeme(path = path, "aeme.yaml")
model_controls <- get_model_controls()
inf_factor = c("glm_aed" = 1)
outf_factor = c("glm_aed" = 1)
model <- c("glm_aed")
build_aeme(path = path, aeme = aeme, model = model,
               model_controls = model_controls, inf_factor = inf_factor, ext_elev = 5,
               use_bgc = FALSE)
#> ℹ Using observed water level
#> ! Missing values in observed water level
#> ℹ Insufficient water level observations. Using constant water level
#> ℹ Correcting water balance using estimated outflows (method = 2).
#> ℹ Calculating lake level using lake depth and a sinisoidal function.
#> ℹ Building GLM-AED2 for lake wainamu
#> ℹ Copied in GLM nml file
#> ✔ GLM nml validation completed — no issues detected.
#>             AEME 
#> -------------------------------------------------------------------
#>   Lake
#> Wainamu (ID: 45819); Lat: -36.89; Lon: 174.47; Elev: 23.64m; Depth: 13.07m;
#> Area: 152343 m2
#> -------------------------------------------------------------------
#>   Time
#> Start: 2020-08-01; Stop: 2021-06-30; Time step: 3600
#>  Spin up (days): GLM: 2; GOTM: 1; DYRESM: 1
#> -------------------------------------------------------------------
#>   Configuration
#>     Model controls: Present
#>     Use biogeochemical model: No
#>           Physical   |   Biogeochemical
#> DY-CD    : Absent     |   Absent 
#> GLM-AED  : Present    |   Absent 
#> GOTM-WET : Absent     |   Absent 
#> -------------------------------------------------------------------
#>   Observations
#> Lake: Present; Level: Present
#> -------------------------------------------------------------------
#>   Input
#> Inital profile: Present; Inital depth: 13.07m; Hypsograph: Present (n=44);
#> Meteo: Present; Use longwave: TRUE; Kw: 1.31
#> -------------------------------------------------------------------
#>   Inflows
#> Data: Present; Scaling factors: DY-CD: 1; GLM-AED: 1; GOTM-WET: 1
#> -------------------------------------------------------------------
#>   Outflows
#> Data: Present; Scaling factors: DY-CD: 1; GLM-AED: 1; GOTM-WET: 1
#> -------------------------------------------------------------------
#>   Water balance
#> Method: 2; Use: obs; Modelled: Absent; Water balance: Present
#> -------------------------------------------------------------------
#>   Parameters: 
#> Number of parameters: 0
#> -------------------------------------------------------------------
#>   Output: 
#> 
#> DY-CD:    
#> GLM-AED:  
#> GOTM-WET: 
```
