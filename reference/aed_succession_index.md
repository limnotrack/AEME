# Compute a system-level phytoplankton succession index

A diagnostic score for whether a run shows genuine, recurring,
multi-group phytoplankton succession – computable purely from model
output, no observations required, so it can be used as a PEST
regularisation target (biasing a calibration away from
single-group-monopoly or one-way-drift solutions) alongside whatever
data-fit objective exists, or just as a diagnostic on a free-running
scenario.

## Usage

``` r
aed_succession_index(
  aeme,
  model,
  groups = c("cyano", "green", "diatom"),
  depth = 0,
  ens_n = 1
)
```

## Arguments

- aeme:

  Aeme object.

- model:

  character vector; models to use. One or more of `"dy_cd"`,
  `"glm_aed"`, `"gotm_wet"`, `"simstrat_aed2"`. Defaults to all models
  if not found in `aeme`.

- groups:

  character vector of phytoplankton group names, matching the
  `PHY_<group>` variables simulated in `aeme` (e.g.
  `c("cyano", "green", "diatom")` matches `PHY_cyano`, `PHY_green`,
  `PHY_diatom`).

- depth:

  numeric; depth (m, surface-referenced) at which each group's biomass
  is evaluated. Default `0` (surface).

- ens_n:

  integer; ensemble member to use. Default `1`.

## Value

a list:

- evenness - mean Pielou's J over the simulated (post-spin-up) period,
  in \[0, 1\]. 0 = permanent single-group monopoly at every timestep. 1
  = every group always holds an equal biomass share.

- dom_entropy - Shannon entropy of the monthly-dominant-group series,
  normalised to \[0, 1\] by dividing by ln(S). 0 = one group wins every
  single month for the whole run. 1 = every group wins an equal share of
  months.

- periodicity - `match_rate * (1 - regime_shift)`, in \[0, 1\]. Near 1 =
  the same group reliably wins the same calendar month every year,
  consistently across the whole record. Near 0 = no reliable
  calendar-month pattern, or a pattern that isn't stable across the
  record.

- match_rate - fraction of months where the dominant group matches the
  modal (most common) dominant group for that calendar month, across all
  years.

- regime_shift - total-variation distance, in \[0, 1\], between the
  dominant-group frequency distribution in the first vs. second half of
  the record. Near 0 = consistent throughout. Near 1 = the record is
  really two different eras (a one-way transient), not one repeating
  cycle.

- composite - geometric mean of `evenness`, `dom_entropy`, and
  `periodicity` – 0 if any component is 0. Use the three components
  individually for diagnosis; use `composite` only as a single PEST
  regularisation observation.

- monthly - the underlying monthly group-fraction/dominant-group table,
  for inspection/plotting.

## Details

Three components are returned separately rather than collapsed into one
number, because they answer three different questions and a single
scalar can be gamed by satisfying only one of them:

- **evenness** - Pielou's J, time-averaged. Is biomass actually shared
  across groups, or is one group always ~100%?

- **dom_entropy** - Shannon entropy of the "which group is dominant this
  month" series. Across the whole run, is *winning* itself spread across
  groups, or does one group win almost every month?

- **periodicity** - does the same group reliably win the same calendar
  month across different years, and is that pattern stable across the
  whole record (see Details for why this is not simple autocorrelation)?

A high `composite` score needs all three: genuine multi-group
coexistence, genuinely shared dominance, and a repeating annual rhythm.
Any one alone is not sufficient.

A one-way transient (e.g. a spin-up-relaxation artifact where the system
slowly drifts from one group's dominance to another's, and never
switches back) can score deceptively well on `evenness` and
`dom_entropy` alone – both groups get real biomass, both win real months
– while `periodicity` stays near zero, because the pattern never
repeats; it just drifts from one regime to another once. That's why
periodicity is a separate, required component rather than folded away:
it's the only one of the three that distinguishes recurring succession
from a slow equilibration.

An earlier design for the periodicity component used lag-12
autocorrelation of each group's continuous fractional-share series. That
failed validation: it scored a confirmed one-way transient almost as
"periodic" as a genuinely recurring pattern, including a flat
single-group monopoly. The reason is structural, not a detrending bug:
light/temperature forcing gives essentially every scenario a real annual
bloom-timing signal regardless of which group is blooming, so a single
group's continuous share is self-similar at lag 12 whether or not the
*identity* of the dominant group is actually what's repeating.
Autocorrelation of a continuous series can't distinguish "the same group
wins every year" from "a bloom happened on the same calendar schedule
while the system was transitioning between two states." The
`match_rate`/`regime_shift` construction works directly on which group
wins instead, and is what should be used or extended if this index needs
further work.

This index cannot tell you whether the *timing* is right (diatom
blooming in spring vs. diatom blooming in autumn) – only that a
plausible-looking, periodic, multi-group pattern exists at all. Treat a
high composite score as necessary, not sufficient; it does not replace
calibration against real community-composition data where that is
available.
