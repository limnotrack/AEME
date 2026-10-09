# Summarise what the observations actually contain

Reports, for every observed variable: how many observations there are,
when they span, how they are distributed across years and seasons, and
whether they are depth *profiles* or single-depth samples.

The profile question is the one that usually decides whether a
calibration is possible. A variable sampled hundreds of times can still
be useless for constraining stratification if every one of those samples
is a surface grab, and observation counts alone hide that completely.

Each variable is classified as one of

- `"profile"`:

  at least half its sampling dates carry `min_depths` or more distinct
  depths - a depth-resolved record.

- `"discrete"`:

  depths are recorded, but most visits carry fewer than `min_depths` of
  them - surface grabs, or surface/bottom pairs.

- `"scalar"`:

  no depth information at all, e.g. water level.

The summary also carries the date range of every *forcing* series - met,
inflows, outflows - because those, not the observations, are usually
what limits how early a simulation can start. See
[`suggest_sim_period()`](https://limnotrack.com/reference/suggest_sim_period.md),
which turns this summary into a runnable period.

## Usage

``` r
summarise_obs(aeme, vars_sim = NULL, min_depths = 3L)
```

## Arguments

- aeme:

  Aeme object.

- vars_sim:

  Character. Variables (`var_aeme` values) to summarise. Default `NULL`
  summarises every observed variable.

- min_depths:

  Integer. Distinct depths a visit needs before it counts as a profile.
  Default `3L`.

## Value

An object of class `aeme_obs_summary`: a list with

- `variables`:

  one row per variable - `n_obs`, `n_dates`, `first`, `last`, `n_years`,
  `n_months` (distinct calendar months, i.e. seasonal coverage),
  `n_profiles`, `profile_frac`, `median_depths`, `depth_min`,
  `depth_max` and `kind`.

- `years`:

  one row per variable per calendar year - `n_obs`, `n_dates`,
  `n_profiles`.

- `forcing`:

  one row per forcing series - `source`, `first`, `last`, `n`.

- `window`:

  the date range over which every forcing series has data, i.e. the
  widest period the model can be run over.

## See also

[`suggest_sim_period()`](https://limnotrack.com/reference/suggest_sim_period.md),
[`set_sim_period()`](https://limnotrack.com/reference/set_sim_period.md),
[`get_obs()`](https://limnotrack.com/reference/get_obs.md)

## Examples

``` r
if (FALSE) { # \dontrun{
aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
s <- summarise_obs(aeme)
s
s$variables
s$years[s$years$var_aeme == "HYD_temp", ]
} # }
```
