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
#> 
#> ── Calculating water balance ──
#> 
#> Resolving water level
#>   ℹ Using observed water level
#> ! Missing values in observed water level
#> ℹ Estimating surface water temperature
#> ✔ Estimating surface water temperature [34ms]
#> 
#> Estimating lake water levels for glm_aed
#>   ℹ Optimizing parameters for water balance
#>   ✔ Optimization Complete: C = 0.3343, h_inv = 23.4915, Final RMSE = 0.1431
#> ℹ Correcting water balance using estimated outflows (method = 2).
#> 
#> ── Building GLM-AED for lake wainamu ──
#> 
#> ℹ Copied in GLM nml file
#> ℹ Copied in AED nml file and supporting files
#> ℹ Copied in GLM plots nml file
#> ✔ GLM nml validation completed - no issues detected.

# Enable biogeochemistry
aeme <- aeme |>
  build_aeme(path = path, model = "glm_aed", model_controls = model_controls,
             ext_elev = 5, use_bgc = TRUE)
#> Warning: ! `SIL_rsi`: SIL_rsi is constant across all rows — this may be a placeholder
#>   value.
#> ℹ Check raw data or unit conversion for this variable.
#> 
#> ── Calculating water balance ──
#> 
#> Resolving water level
#>   ℹ Using observed water level
#> ! Missing values in observed water level
#> ℹ Estimating surface water temperature
#> ✔ Estimating surface water temperature [33ms]
#> 
#> Estimating lake water levels for glm_aed
#> ℹ Correcting water balance using estimated outflows (method = 2).
#> 
#> ── Building GLM-AED for lake wainamu ──
#> 
#> ℹ No variables to initialise in AED
#> ✔ Updated GLM-AED models from: aed_sedflux, aed_oxygen, aed_silica,
#>   aed_nitrogen, aed_phosphorus, aed_organic_matter, aed_phytoplankton,
#>   aed_zooplankton, aed_macrophyte, aed_totals to: aed_sedflux, aed_oxygen,
#>   aed_silica, aed_nitrogen, aed_phosphorus, aed_organic_matter,
#>   aed_phytoplankton, aed_totals
#> ℹ Setting up AED aed_sed_const2d sediment zones: 2
#> ℹ Tier 2: zone-median summer concentrations used for adjustment:
#> ┌─────┬───────────┬────────────┬────────────┬────────────┐
#> │ zone│ O2 (mg/L) │ NH4 (mg/L) │ NO3 (mg/L) │ FRP (mg/L) │
#> ├─────┼───────────┼────────────┼────────────┼────────────┤
#> │Zone1│ 0.075     │ 0.078      │ 0.01       │ 0.004      │
#> │Zone2│ 7.16      │ 0.005      │ 0.001      │ 0.002      │
#> └─────┴───────────┴────────────┴────────────┴────────────┘
#> ℹ Tier 2 adjustments applied: fsed_amm (2 zones, direct NH4); fsed_frp (2
#>   zones, direct FRP)
#> ── Sediment zone flux estimates (obs_adjusted) ─────────────────────────────────
#> n_zones: 2 | max lake depth: 13.07 m | ref_depth: 5 m
#> ┌────┬───────────┬───────────┬───────────┬───────────┬──────────┬─────────┬─────────┬─────┬─────┬────┬──────┐
#> │Zone│H lower (m)│H upper (m)│D upper (m)│D lower (m)│Mean D (m)│Area (m2)│Area frac│ O2  │ NH4 │ NO3│ FRP  │
#> ├────┼───────────┼───────────┼───────────┼───────────┼──────────┼─────────┼─────────┼─────┼─────┼────┼──────┤
#> │   1│    0      │ 3.07      │   10      │ 13.1      │ 11.5     │ 4.4e+04 │ 0.289   │-38.8│ 5.83│-0.4│ 0.103│
#> │   2│ 3.07      │   19      │    0      │   10      │    5     │ 1.08e+05│ 0.711   │-19.4│0.512│ 0.1│0.0259│
#> └────┴───────────┴───────────┴───────────┴───────────┴──────────┴─────────┴─────────┴─────┴─────┴────┴──────┘
#> 
#> ── Lake-wide area-weighted average fluxes ──────────────────────────────────────
#> ┌──────────────┬───────────────┬───────────────┬───────────────┐
#> │O2 (mmol/m2/d)│NH4 (mmol/m2/d)│NO3 (mmol/m2/d)│FRP (mmol/m2/d)│
#> ├──────────────┼───────────────┼───────────────┼───────────────┤
#> │ -25.007      │ 2.05          │ -0.044        │ 0.048         │
#> └──────────────┴───────────────┴───────────────┴───────────────┘
#> ✔ GLM nml validation completed - no issues detected.
```
