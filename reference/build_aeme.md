# Build model configuration directories

Configure an ensemble of lake model simulations from a basic set of
inputs.

## Usage

``` r
build_aeme(
  aeme = NULL,
  model = NULL,
  path = NULL,
  use_bgc = NULL,
  ext_elev = NULL,
  wb_method = NULL,
  model_controls = NULL,
  inf_factor = NULL,
  outf_factor = NULL,
  calc_wbal = NULL,
  calc_wlev = NULL,
  coeffs = NULL,
  hum_type = NULL,
  est_swr_hr = NULL,
  use_aeme = FALSE,
  config = NULL
)
```

## Arguments

- aeme:

  Aeme object.

- model:

  character vector; models to use. One or more of `"dy_cd"`,
  `"glm_aed"`, `"gotm_wet"`. Defaults to all models if not found in
  `aeme`.

- path:

  character; directory where input files are located. Defaults to the
  path stored in `aeme`, or the current working directory if not set.

- use_bgc:

  logical; enable the biogeochemical model. Default: `FALSE`.

- ext_elev:

  numeric; elevation (m) to extend the hypsograph to. Default: `0`.

- wb_method:

  integer; water balance method. One of:

  - `1` — no inflows or outflows

  - `2` — outflows calculated (default)

  - `3` — unexplained storage changes treated as effective
    inflows/outflows

- model_controls:

  data.frame; model configuration, typically loaded via
  [`get_model_controls()`](https://limnotrack.com/reference/get_model_controls.md).

- inf_factor:

  named numeric vector; factors to multiply inflows by, named by model.

- outf_factor:

  named numeric vector; factors to multiply outflows by, named by model.

- calc_wbal:

  logical; calculate water balance. Default: `TRUE`.

- calc_wlev:

  logical; calculate water level. Default: `TRUE`.

- coeffs:

  numeric vector of length 2; coefficients for estimating surface water
  temperature when calculating evaporation. If water temperature
  observations are present in `aeme`, a linear model is fitted against
  air temperature. Otherwise defaults to \\T\_{water} = 5 + 0.75 \times
  T\_{air}\\ (Stefan & Preud'homme, 1993,
  [doi:10.1111/j.1752-1688.1993.tb01502.x](https://doi.org/10.1111/j.1752-1688.1993.tb01502.x)
  ).

- hum_type:

  integer; humidity input type for GOTM. One of:

  - `1` — relative humidity (%)

  - `2` — wet-bulb temperature

  - `3` — dew point temperature (default)

  - `4` — specific humidity (kg/kg)

- est_swr_hr:

  logical; estimate hourly shortwave radiation from daily values.
  Default: `TRUE`.

- use_aeme:

  logical; use the `aeme` object to generate model configuration files.
  Default: `FALSE`.

- config:

  list; AEME configuration, typically loaded via
  `yaml::read_yaml("aeme.yaml")`.

## Value

An updated `aeme` object.

## Examples

``` r
aeme_dir <- system.file("extdata/lake/", package = "AEME")
path <- "aeme"
aeme <- yaml_to_aeme(path = aeme_dir, "aeme.yaml")
model_controls <- get_model_controls()

# Build configuration for GLM-AED
aeme <- aeme |>
  build_aeme(path = path, model = "glm_aed", model_controls = model_controls,
             ext_elev = 5)
#> ✔ Created missing directory: D:\a\AEME\AEME\docs\reference\aeme
#> Warning: ! `SIL_rsi`: SIL_rsi is constant across all rows — this may be a placeholder
#>   value.
#> ℹ Check raw data or unit conversion for this variable.
#> ℹ Using observed water level.
#> ! Missing values in observed water level.
#> ℹ Correcting water balance using estimated outflows (method = 2).
#> ℹ Building GLM-AED for lake wainamu
#> ℹ Copied in GLM nml file
#> ℹ Copied in AED nml file and supporting files
#> Error in rename_modelvars(var_names, type_output = "glm_aed"): `input` must be a non-empty character vector.

# Enable biogeochemistry
aeme <- aeme |>
  build_aeme(path = path, model = "glm_aed", model_controls = model_controls,
             ext_elev = 5, use_bgc = TRUE)
#> Warning: ! `SIL_rsi`: SIL_rsi is constant across all rows — this may be a placeholder
#>   value.
#> ℹ Check raw data or unit conversion for this variable.
#> ℹ Using observed water level.
#> ! Missing values in observed water level.
#> ℹ Correcting water balance using estimated outflows (method = 2).
#> ℹ Building GLM-AED for lake wainamu
#> Error in rename_modelvars(var_names, type_output = "glm_aed"): `input` must be a non-empty character vector.
```
