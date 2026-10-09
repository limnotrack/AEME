# Options for the AED biogeochemistry setup in `build_aeme()`

With `use_bgc = TRUE`,
[`build_aeme()`](https://limnotrack.com/reference/build_aeme.md)
configures AED for GLM-AED in several steps. `aed_options()` lets you
choose which of these run. The defaults reproduce the usual build, so
only set what you want to change. Every step is also available on its
own once the model is built:
[`set_glm_aed_models()`](https://limnotrack.com/reference/set_glm_aed_models.md),
[`set_aed_sed_const2d()`](https://limnotrack.com/reference/set_aed_sed_const2d.md)
and
[`set_aed_totals()`](https://limnotrack.com/reference/set_aed_totals.md).

## Usage

``` r
aed_options(
  modules = NULL,
  resolve_deps = TRUE,
  initialise = TRUE,
  sed_zones = TRUE,
  totals = TRUE
)
```

## Arguments

- modules:

  character or `NULL`; the AED modules to activate, from
  `"aed_sedflux"`, `"aed_noncohesive"`, `"aed_oxygen"`, `"aed_silica"`,
  `"aed_nitrogen"`, `"aed_phosphorus"`, `"aed_alum"`,
  `"aed_organic_matter"`, `"aed_phytoplankton"`, `"aed_zooplankton"`,
  `"aed_macrophyte"` and `"aed_totals"`. `NULL` (default) activates the
  modules needed for the variables set to simulate in `model_controls`.

- resolve_deps:

  logical; when `modules` is supplied, also activate the modules they
  depend on (e.g. `aed_nitrogen` needs `aed_oxygen` and `aed_sedflux`),
  as the `model_controls`-driven default does. If `FALSE`, `modules` is
  used exactly as given, and GLM aborts at runtime if a module's
  prerequisites are missing. Default `TRUE`.

- initialise:

  logical; write the initial concentrations and active modules to
  `aed.nml` from `model_controls`. If `FALSE`, `aed.nml` is left as it
  is (the shipped template for a new lake, or your own file for an
  existing one). Default `TRUE`.

- sed_zones:

  logical; estimate and set the per-zone sediment fluxes
  (`aed_sed_const2d`) with
  [`set_aed_sed_const2d()`](https://limnotrack.com/reference/set_aed_sed_const2d.md).
  Default `TRUE`.

- totals:

  logical; derive the `aed_totals` (TN, TP, TOC and TSS) variable lists
  with
  [`set_aed_totals()`](https://limnotrack.com/reference/set_aed_totals.md).
  Default `TRUE`.

## Value

An `aed_options` object, to pass as the `aed` argument of
[`build_aeme()`](https://limnotrack.com/reference/build_aeme.md).

## See also

[`build_aeme()`](https://limnotrack.com/reference/build_aeme.md),
[`set_glm_aed_models()`](https://limnotrack.com/reference/set_glm_aed_models.md),
[`set_aed_sed_const2d()`](https://limnotrack.com/reference/set_aed_sed_const2d.md),
[`set_aed_totals()`](https://limnotrack.com/reference/set_aed_totals.md)

## Examples

``` r
# Default: everything as usual
aed_options()
#> <aed_options>
#> • Modules: from `model_controls`
#> • Write initial concentrations to aed.nml: TRUE
#> • Estimate sediment zone fluxes: TRUE
#> • Derive aed_totals: TRUE

# Only oxygen and nutrient cycling, and leave the sediment fluxes alone
aed_options(
  modules = c("aed_sedflux", "aed_oxygen", "aed_nitrogen", "aed_phosphorus"),
  sed_zones = FALSE
)
#> <aed_options>
#> • Modules: "aed_sedflux", "aed_oxygen", "aed_nitrogen", and "aed_phosphorus"
#>   plus their dependencies
#> • Write initial concentrations to aed.nml: TRUE
#> • Estimate sediment zone fluxes: FALSE
#> • Derive aed_totals: TRUE
```
