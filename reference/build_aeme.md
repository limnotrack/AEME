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
tmpdir <- tempdir()
aeme_dir <- system.file("extdata/lake/", package = "AEME")
# Copy files from package into tempdir
file.copy(aeme_dir, tmpdir, recursive = TRUE)
#> [1] TRUE
path <- file.path(tmpdir, "lake")
aeme <- yaml_to_aeme(path = path, "aeme.yaml")
#> Warning: ! `lake$id` was not a <character> and was coerced.
#> ℹ Supply `lake$id` as a character string to avoid this.
model_controls <- get_model_controls()
model <- c("glm_aed")
build_aeme(path = path, aeme = aeme, model = model,
               model_controls = model_controls, inf_factor = inf_factor, ext_elev = 5,
               use_bgc = FALSE)
#> Error: object 'inf_factor' not found
```
