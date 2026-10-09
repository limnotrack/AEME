# Reconstruct an Aeme object from a GLM-AED model configuration

The inverse of
[`build_aeme()`](https://limnotrack.com/reference/build_aeme.md) for the
GLM-AED model. Parses an existing GLM hydrodynamic nml file (`glm3.nml`,
`glm4.nml`, or any future `glm<version>.nml`; see
[`find_glm_nml()`](https://limnotrack.com/reference/find_glm_nml.md))
and the sibling files it references (meteorology, inflows, outflows, AED
biogeochemistry) and reassembles them into an `Aeme` object.

## Usage

``` r
glm_config_to_aeme(
  nml_file,
  model_controls = NULL,
  spin_up = 2,
  read_params = FALSE
)
```

## Arguments

- nml_file:

  character; path to a GLM hydrodynamic nml file (e.g. `glm3.nml`,
  `glm4.nml`), typically inside a `<id>_<name>/glm_aed/` directory
  written by
  [`build_aeme()`](https://limnotrack.com/reference/build_aeme.md).

- model_controls:

  data.frame; model configuration, typically loaded via
  [`get_model_controls()`](https://limnotrack.com/reference/get_model_controls.md).
  If `NULL` (default), one is generated with
  [`get_model_controls()`](https://limnotrack.com/reference/get_model_controls.md),
  using biogeochemistry state (see Details) to set `use_bgc`.

- spin_up:

  numeric; number of spin-up days assumed to have been subtracted from
  `time$start` when the nml file was written (see Details). Default `2`,
  matching
  [`aeme_constructor()`](https://limnotrack.com/reference/aeme_constructor.md)'s
  own default.

- read_params:

  logical; also recover a `parameters` data frame (as set by
  [`add_param()`](https://limnotrack.com/reference/add_param.md)) by
  cross-referencing every parameter known to
  [`get_aeme_parameters()`](https://limnotrack.com/reference/get_aeme_parameters.md)
  against the value actually present in the GLM nml, `aed/aed.nml`, and
  the AED parameter CSVs. Default `FALSE`.

## Value

An `Aeme` object.

## Details

Several pieces of the original `Aeme` object cannot be recovered exactly
from the GLM-AED files alone, and are approximated:

- `lake$id` is taken from the `<id>_<name>` lake directory naming
  convention used by
  [`build_aeme()`](https://limnotrack.com/reference/build_aeme.md) (the
  parent of `nml_file`'s directory). If the directory does not follow
  that convention, `"0001"` is used and a warning is issued.

- `lake$name` is whatever is stored in `morphometry$lake_name`, which
  [`build_aeme()`](https://limnotrack.com/reference/build_aeme.md)
  always writes in lower case – the original capitalisation is not
  recoverable.

- `lake$elevation` (and the hypsograph's elevation datum) is taken as
  `crest_elev`, i.e. the top of the hypsograph as written to the nml. If
  the hypsograph was extended with `ext_elev` at build time, this will
  not match the original `lake$elevation`.

- `time$start` has `spin_up` days added back on, since
  [`build_aeme()`](https://limnotrack.com/reference/build_aeme.md)
  subtracts each model's spin-up period from `time$start` before writing
  it to the nml file. The true original spin-up is not stored anywhere
  in the GLM-AED files, so `spin_up` is a caller-supplied guess
  (default:
  [`aeme_constructor()`](https://limnotrack.com/reference/aeme_constructor.md)'s
  own default of 2 days).

- `inflows()$factor` / `outflows()$factor` are assumed to be `1` – any
  factor applied at build time is already baked into the written `.csv`
  values and cannot be separated back out.

- `parameters` (when `read_params = TRUE`) only recovers scalar, numeric
  parameters known to AEME's parameter catalogue
  ([`get_aeme_parameters()`](https://limnotrack.com/reference/get_aeme_parameters.md));
  `min`/`max`/`group` come from that catalogue, not from the
  lake-specific files (which do not store them). Logical/character nml
  values (e.g. `glm_setup::non_avg`) and vector-valued/indexed nml
  parameters (e.g. per sediment zone) are skipped, since the catalogue
  has no lake-specific notion of a vector's true length and recovering
  only part of it would corrupt the field if written back via
  [`input_model_parameters()`](https://limnotrack.com/reference/input_model_parameters.md).

The returned object has `configuration()$calc_wbal`, `calc_wlev`, and
`ext_elev` set to `FALSE`/`FALSE`/`0`, rather than
[`build_aeme()`](https://limnotrack.com/reference/build_aeme.md)'s own
defaults of `TRUE`/`TRUE`/`0`. The loaded
[`inflows()`](https://limnotrack.com/reference/inflows.md)/[`outflows()`](https://limnotrack.com/reference/outflows.md)/hypsograph
already reflect a finished water balance and lake-level calculation;
leaving `calc_wbal`/`calc_wlev` at their
[`build_aeme()`](https://limnotrack.com/reference/build_aeme.md)
defaults would make a subsequent `build_aeme(aeme = ., use_aeme = TRUE)`
call recompute and silently overwrite those loaded values
(`use_aeme = TRUE` only preserves the raw nml text – it does not by
itself disable the water-balance/lake-level recalculation that runs
before the per-model config files are written). Passing
`calc_wbal`/`calc_wlev`/`ext_elev` explicitly to a later
[`build_aeme()`](https://limnotrack.com/reference/build_aeme.md) call
overrides these stored values as usual.

The GLM version this configuration was read from (e.g. `"glm3.nml"` or
`"glm4.nml"`) is recorded in
`configuration()$glm_aed$hydrodynamic_file`, so a later
[`write_configuration()`](https://limnotrack.com/reference/write_configuration.md)
or [`build_aeme()`](https://limnotrack.com/reference/build_aeme.md) call
writes it back out under the same filename rather than assuming
`glm3.nml`.

## Examples

``` r
aeme_dir <- system.file("extdata/lake/", package = "AEME")
path <- file.path(tempdir(), "glm_config_to_aeme_example")
aeme <- yaml_to_aeme(path = aeme_dir, "aeme.yaml")
model_controls <- get_model_controls()
aeme <- aeme |>
  build_aeme(path = path, model = "glm_aed", model_controls = model_controls,
             ext_elev = 5)
#> ✔ Created missing directory:
#>   C:\Users\RUNNER~1\AppData\Local\Temp\Rtmps9nlgV\glm_config_to_aeme_example
#> Warning: ! `SIL_rsi`: SIL_rsi is constant across all rows -- this may be a placeholder
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
#>   ℹ Optimizing parameters for water balance
#>   ✔ Optimization Complete: C = 0.3343, h_inv = 23.4915, Final RMSE = 0.1431
#> ℹ Correcting water balance using estimated outflows (method = 2).
#> 
#> ── Building GLM-AED for lake wainamu ──
#> 
#> ℹ Copied in GLM nml file (glm4.nml)
#> ℹ Copied in AED nml file and supporting files
#> ℹ Copied in GLM plots nml file
#> ! Forcing sed_heat_model from 2 to 1: sed_heat_model = 2 needs an active WQ
#>   module and `use_bgc` is FALSE.
#> ✔ GLM nml validation completed - no issues detected.
nml_file <- find_glm_nml(file.path(get_lake_dir(aeme, path), "glm_aed"))
aeme2 <- glm_config_to_aeme(nml_file)
```
