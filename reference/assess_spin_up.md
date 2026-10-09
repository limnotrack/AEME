# Assess model sensitivity to initial conditions and required spin-up

Build and run an ensemble of simulations that differ only in their
initial conditions, repeated across a range of spin-up lengths, then
measure how the ensemble spread at the start of the analysis period
collapses as spin-up is extended. The shortest spin-up at which the
spread falls below `tolerance` is the length beyond which the choice of
initial condition no longer materially affects the reported variables.

## Usage

``` r
assess_spin_up(
  aeme,
  model,
  spin_up = c(0, 30, 90, 180, 365),
  perturb = list(temperature = c(-3, 0, 3)),
  vars = "HYD_temp",
  metric = c("spread", "spread_cv", "drift", "drift_cv"),
  model_controls = NULL,
  path = tempdir(),
  tolerance = NULL,
  build_args = list(),
  run_args = list(),
  verbose = FALSE
)
```

## Arguments

- aeme:

  Aeme object.

- model:

  character(1); the model to assess.

- spin_up:

  numeric; spin-up lengths in days to test. Default
  `c(0, 30, 90, 180, 365)`.

- perturb:

  named list describing the initial-condition ensemble (see Details).
  Default `list(temperature = c(-3, 0, 3))`.

- vars:

  character; AEME variable names to report and restrict written model
  output to. Default `"HYD_temp"`.

- metric:

  character(1); which summary column the recommendation (and
  `tolerance`) applies to. One of `"spread"` (default; across-member SD
  in the variable's units), `"spread_cv"` (that SD divided by the
  ensemble mean, per depth - dimensionless, comparable across
  variables), `"drift"` or `"drift_cv"` (the same normalisation applied
  to the distance from the longest-spin-up ensemble mean). All four are
  always present in `summary`; `metric` only selects the one used for
  `recommended`. The `*_cv` forms are meant for strictly-positive
  variables (water-quality concentrations, salinity, K); a CV is
  ill-defined where the ensemble mean crosses zero and those depths are
  dropped from the depth-average.

- model_controls:

  data.frame; model configuration, typically loaded via
  [`get_model_controls()`](https://limnotrack.com/reference/get_model_controls.md).

- path:

  character; directory under which the per-run subdirectories are
  created. Default [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- tolerance:

  numeric(1); threshold on `metric` (a named vector, one per variable,
  is allowed) used to pick the recommended spin-up. `NULL` (default)
  skips the recommendation.

- build_args, run_args:

  lists of extra arguments passed to
  [`build_aeme()`](https://limnotrack.com/reference/build_aeme.md) and
  [`run_aeme()`](https://limnotrack.com/reference/run_aeme.md)
  respectively.

- verbose:

  logical; print model progress. Default `FALSE`.

## Value

A list of class `aeme_spin_up` with:

- `data` - long data.frame: `spin_up`, `member`, `var`, `depth`, `value`
  (state at the start of the analysis period).

- `summary` - `spin_up`, `var`, and, each as a mean over depth of the
  per-depth quantity: `spread` (across-member SD), `spread_cv` (SD /
  \|ensemble mean\|), `drift` (\|this spin-up's mean - longest spin-up's
  mean\|) and `drift_cv` (that / \|longest spin-up's mean\|).

- `recommended` - named numeric (per `var`) shortest spin-up whose
  `metric` meets `tolerance`, and `overall` the maximum of those; `NULL`
  if `tolerance` is `NULL`.

- `model`, `vars`, `metric`, `tolerance`, `failures`.

## Details

Each ensemble member is a perturbation of the baseline initial
conditions (`get_initial_conditions(aeme, model = model)`, falling back
to `input(aeme)$init_profile` / `model_controls$initial_wc`). `perturb`
is a named list:

- `"temperature"` / `"salt"` - numeric vector of **additive** offsets
  applied to that column of the initial profile.

- any `model_controls$var_aeme` name - numeric vector of
  **multiplicative** factors applied to that water-quality initial
  value.

The vectors are recycled to a common length, which becomes the ensemble
size; member `i` takes element `i` of every vector.

One [`build_aeme()`](https://limnotrack.com/reference/build_aeme.md) +
[`run_aeme()`](https://limnotrack.com/reference/run_aeme.md) is
performed per (spin-up, member) combination, each in its own
subdirectory of `path`, with written output restricted to `vars`. This
can be a large number of model runs.

## See also

[`set_initial_conditions()`](https://limnotrack.com/reference/set_initial_conditions.md),
[`plot_spin_up()`](https://limnotrack.com/reference/plot_spin_up.md)

## Examples

``` r
if (FALSE) { # \dontrun{
aeme_dir <- system.file("extdata/lake/", package = "AEME")
aeme <- yaml_to_aeme(path = aeme_dir, "aeme.yaml")
res <- assess_spin_up(aeme, model = "glm_aed",
                      spin_up = c(0, 30, 90, 180),
                      perturb = list(temperature = c(-4, 0, 4)),
                      tolerance = 0.5)
res$recommended
plot_spin_up(res)
} # }
```
