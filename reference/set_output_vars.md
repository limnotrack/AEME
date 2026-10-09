# Restrict a model's written output to the variables of interest

By default every AEME model writes its full state at every output step.
When only a few variables are needed - as in calibration or sensitivity
analysis, where the objective is computed from one or two variables -
the rest is wasted disk I/O (and, for Simstrat and GOTM, wasted files).
`set_output_vars()` rewrites the output section of a model's
configuration so that only `vars`, plus the handful of internals AEME
always needs to read a result back (water level, the depth grid,
temperature), are written.

## Usage

``` r
set_output_vars(aeme, model, vars, mass_balance = TRUE, ens_n = 1)
```

## Arguments

- aeme:

  An `Aeme` object carrying a configuration for `model`.

- model:

  Character. A single model, one of `"glm_aed"`, `"gotm_wet"`,
  `"simstrat_aed"`, `"simstrat_aed2"`, `"dy_cd"`.

- vars:

  Character. AEME variable names to keep, e.g.
  `c("HYD_temp", "CHM_oxy")`. Mapped to each model's own output names
  via [`key_naming`](https://limnotrack.com/reference/key_naming.md);
  names with no mapping for `model` are dropped with a warning.

- mass_balance:

  Logical. For `"glm_aed"` only: keep the GLMv4 `&mass_balance`
  diagnostic CSV? Default `TRUE`. Ignored for other models.

- ens_n:

  Integer. Ensemble member whose configuration slot is updated. Default
  `1`.

## Value

`aeme`, with its
[`configuration()`](https://limnotrack.com/reference/configuration.md)
updated.

## Details

The change is made on the in-memory configuration; call
[`write_configuration`](https://limnotrack.com/reference/write_configuration.md)
(or [`build_aeme`](https://limnotrack.com/reference/build_aeme.md)) to
write it to disk.
[`build_aeme`](https://limnotrack.com/reference/build_aeme.md)'s
`output_vars` argument applies this automatically at build time.

- **GLM** always writes the full netCDF - its gridded variables cannot
  be sub-selected - so this only drops the `&output` `csv_point_*` keys
  (disabling the fixed-depth `WQ_*.csv` files) and, when
  `mass_balance = FALSE`, the whole `&mass_balance` block (disabling
  `mass_balance.csv`). The whole-lake `lake.csv` (`csv_lake_fname`) is
  left on: GLM 4.x only writes the netCDF diagnostic scalars
  (`lake_level`, ...) while that CSV is open, and AEME needs
  `lake_level` to read a GLM result back.

- **Simstrat** writes one `*_out.dat` per variable; this sets
  `Output$All = FALSE` and lists only the needed variables.

- **GOTM** replaces the `/*` (all-variables) output source with an
  explicit list.

- **DYRESM** has a fixed output form and is returned unchanged.

## See also

[`set_vars_sim`](https://limnotrack.com/reference/set_vars_sim.md),
[`write_configuration`](https://limnotrack.com/reference/write_configuration.md),
[`get_output_vars`](https://limnotrack.com/reference/get_output_vars.md)

## Examples

``` r
aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
path <- tempdir()
aeme <- build_aeme(path = path, aeme = aeme, model = "glm_aed",
                   model_controls = get_model_controls(), ext_elev = 5)
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
#> ✔ Estimating surface water temperature [24ms]
#> 
#> Estimating lake water levels for glm_aed
#>   ℹ Optimizing parameters for water balance
#>   ✔ Optimization Complete: C = 0.3343, h_inv = 23.4915, Final RMSE = 0.1431
#> ℹ Correcting water balance using estimated outflows (method = 2).
#> 
#> ── Building GLM-AED for lake wainamu ──
#> 
#> ✔ GLM nml validation completed - no issues detected.
aeme <- set_output_vars(aeme, "glm_aed", "HYD_temp", mass_balance = FALSE)
write_configuration(aeme, model = "glm_aed", path = path)
```
