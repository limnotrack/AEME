# Set initial conditions for AEME models

Configure the initial state - water depth, temperature and salinity
profile, and biogeochemical water-column pools - that
[`build_aeme()`](https://limnotrack.com/reference/build_aeme.md) writes
into each model's configuration. Values can be supplied once for every
model (`depth`, `profile`, `wq`) and/or overridden per model via
`model_init` (for example a GLM-AED-specific temperature profile).

## Usage

``` r
set_initial_conditions(
  aeme,
  depth = NULL,
  profile = NULL,
  wq = NULL,
  model_init = NULL,
  model_controls = NULL,
  from_obs = FALSE,
  reset = FALSE
)
```

## Arguments

- aeme:

  Aeme object.

- depth:

  numeric(1); initial water depth (m), i.e. the height of the lake
  surface above the lowest point of the hypsograph. `NULL` (default)
  leaves it unchanged.

- profile:

  data.frame; initial profile with a `depth` column (positive-down, 0 =
  surface) and a `temperature` column (degC); a `salt` column (ppt) is
  optional and defaults to 0. `NULL` (default) leaves the profile
  unchanged.

- wq:

  named list; initial water-column values keyed by AEME variable name
  (e.g. `list(CHM_oxy = 300, NIT_amm = 0.5)`). Each element is either a
  single number (constant with depth) or a data.frame with `depth` and
  `value` columns. Names must be present in `model_controls$var_aeme`.
  `NULL` (default) leaves water-quality initial values unchanged.

- model_init:

  named list; per-model overrides keyed by model name (`"dy_cd"`,
  `"glm_aed"`, `"gotm_wet"`, `"simstrat_aed2"`, `"simstrat_aed"`). Each
  element is itself a list with any of `depth`, `profile` and `wq`,
  following the same rules as the generic arguments. These are merged
  over the generic specification when that model is built. `NULL`
  (default) leaves per-model overrides unchanged.

- model_controls:

  data.frame; model configuration, typically loaded via
  [`get_model_controls()`](https://limnotrack.com/reference/get_model_controls.md).

- from_obs:

  logical; if `TRUE`, seed the generic profile and water-quality values
  from lake observations via
  [`update_init()`](https://limnotrack.com/reference/update_init.md)
  before the explicit arguments are applied (the explicit arguments take
  precedence). Default `FALSE`.

- reset:

  logical; if `TRUE`, discard any existing
  `configuration(aeme)$initial_conditions` before applying the
  arguments. Default `FALSE`, i.e. new values are merged into the
  existing specification.

## Value

The `aeme` object with initial conditions set.

## Details

The structured specification is stored in
`configuration(aeme)$initial_conditions` as a list with a `default`
entry plus an optional entry per model, and is resolved at build time by
merging each model's overrides over the defaults. For backwards
compatibility the generic `depth`/`profile` are also written to
`input(aeme)$init_depth` / `input(aeme)$init_profile`, and scalar
generic `wq` values into
`configuration(aeme)$model_controls$initial_wc`, so the generic controls
take effect immediately.

## See also

[`get_initial_conditions()`](https://limnotrack.com/reference/get_initial_conditions.md),
[`update_init()`](https://limnotrack.com/reference/update_init.md),
[`set_glm_init()`](https://limnotrack.com/reference/set_glm_init.md),
[`set_gotm_init()`](https://limnotrack.com/reference/set_gotm_init.md),
[`set_simstrat_init()`](https://limnotrack.com/reference/set_simstrat_init.md)

## Examples

``` r
aeme_file <- system.file("extdata/aeme.rds", package = "AEME")
aeme <- readRDS(aeme_file)

# Generic profile for all models
prof <- data.frame(depth = c(0, 5, 10), temperature = c(18, 14, 11))
aeme <- set_initial_conditions(aeme, depth = 10, profile = prof)
#> ℹ `profile` has no salt column; using 0.
#> ✔ Initial conditions updated.

# GLM-AED-specific override
aeme <- set_initial_conditions(
  aeme,
  model_init = list(
    glm_aed = list(profile = data.frame(depth = c(0, 10),
                                        temperature = c(20, 12)))
  )
)
#> ℹ `model_init$glm_aed$profile` has no salt column; using 0.
#> ✔ Initial conditions updated (1 model-specific override).
```
