# Choose a simulation period from what the data can support

Picks `start` and `stop` dates for a simulation by intersecting three
things: where the forcing data exist, where the observations to be
fitted exist, and how much spin-up is needed before the first
comparison.

The forcing constraint is the one most often missed. Observations
routinely predate the meteorological record by years, and a period
chosen from the observations alone can begin before the model has
anything to run on - or leave no room for spin-up, so the first
observations are compared against a model still relaxing from its
initial condition.

Sparse leading and trailing years are trimmed by `min_density`, measured
in profiles rather than observations, because a depth-resolved
calibration is constrained by casts and not by surface grabs. Only the
ends are trimmed, so a lean year inside a dense record is kept and the
period stays contiguous.

## Usage

``` r
suggest_sim_period(
  aeme,
  vars_sim = NULL,
  spin_up = NULL,
  min_depths = 3L,
  use_profiles = TRUE,
  min_density = 0.25,
  min_years = 4L,
  align = c("none", "year"),
  year_start_month = 7L
)
```

## Arguments

- aeme:

  Aeme object.

- vars_sim:

  Character. Variables the period must cover. Default `NULL` uses every
  observed variable, which is rarely what you want - name the variables
  you intend to fit.

- spin_up:

  Numeric. Days of spin-up required before `start`. Default `NULL` takes
  the longest spin-up already set on the object.

- min_depths:

  Integer. Distinct depths a visit needs before it counts as a profile.
  Default `3L`.

- use_profiles:

  Logical. Count only profiles when judging coverage. Default `TRUE`;
  `FALSE` counts every sampling date. Variables of kind `"scalar"`
  always count by date.

- min_density:

  Numeric. Drop leading and trailing years holding fewer than this
  fraction of the median year's count. Default `0.25`; `0` keeps the
  record whole.

- min_years:

  Integer. Fewest years a record must span before `min_density` is
  applied. Default `4L`.

- align:

  Character. `"none"` (default) starts and stops on observation dates;
  `"year"` snaps outward to whole years beginning `year_start_month`.

- year_start_month:

  Integer. First month of the year used by `align = "year"`. Default
  `7L`, the southern-hemisphere hydrological year, so a period does not
  split a stratified season in half.

## Value

An object of class `aeme_sim_period`: a list with `start`, `stop`,
`spin_up`, `spin_up_start`, `limited_by` (what set each end), `coverage`
(per-variable counts inside the chosen period), `dropped` (years trimmed
at each end) and `summary` (the
[`summarise_obs()`](https://limnotrack.com/reference/summarise_obs.md)
result).

## See also

[`summarise_obs()`](https://limnotrack.com/reference/summarise_obs.md),
[`set_sim_period()`](https://limnotrack.com/reference/set_sim_period.md),
[`set_time()`](https://limnotrack.com/reference/set_time.md)

## Examples

``` r
if (FALSE) { # \dontrun{
aeme <- readRDS(system.file("extdata/aeme.rds", package = "AEME"))
p <- suggest_sim_period(aeme, vars_sim = c("HYD_temp", "CHM_oxy"),
                        spin_up = 365)
p
aeme <- set_sim_period(aeme, p)
} # }
```
