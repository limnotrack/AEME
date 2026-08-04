# GLM-AED: Sediment Nitrogen & Phosphorus Pools and Resuspension

## Purpose

This note summarises how the [AED water quality library (`libaed-water`,
as coupled to
GLM)](https://github.com/AquaticEcoDynamics/libaed-water/tree/9498c5b7390d8e72e1eea32e1bfee0c8d06be84e)
represents sediment nitrogen and phosphorus, and how it simulates
resuspension. It compares two fundamentally different ways of
parameterising these processes and gives guidance on which is
appropriate for multi-decadal climate-scenario runs (e.g. 100-year
simulations).

All statements below are grounded directly in the AED source
(`aed_sedflux.F90`, `aed_nitrogen.F90`, `aed_phosphorus.F90`,
`aed_noncohesive.F90`, `aed_organic_matter.F90`) rather than
documentation, since the namelist documentation does not describe this
behaviour.

## Architecture: how the pieces connect

``` mermaid
flowchart TD
    subgraph physics["Physical resuspension (aed_noncohesive.F90)"]
        A["Bottom shear stress τ_b<br/>(from GLM hydrodynamics)"]
        B["Erosion law:<br/>resus_flux = ε·(τ_b − τ_0)/τ_r<br/>if τ_b > τ_0, else 0"]
        A --> B
    end

    subgraph sedflux["Diffusive exchange (aed_sedflux.F90)"]
        C["Prescribed or zoned flux rates<br/>Fsed_amm, Fsed_nit, Fsed_frp<br/>(constant / constant2d / dynamic)"]
    end

    B -->|"NCS_resus<br/>(shared mass flux, g/m2/s)"| D["Adsorbed P (frp_ads)<br/>aed_phosphorus.F90<br/>× fixed sedpipfr ratio"]
    B -->|"NCS_resus"| E["Particulate organic N, P<br/>aed_organic_matter.F90<br/>× Xsn / Xsp ratios"]
    C -->|"O2/temp-modulated"| F["Dissolved NH4, NO3<br/>aed_nitrogen.F90"]
    C -->|"O2/temp-modulated"| G["Dissolved FRP<br/>aed_phosphorus.F90"]

    D --> H(("Water column"))
    E --> H
    F --> H
    G --> H
    B -->|"NCS_ss (mass)"| H
```

Two mechanisms feed nutrients into the water column from the sediment:

1.  **Diffusive/redox exchange** (`aed_sedflux.F90`) — oxygen- and
    temperature-modulated flux rates for dissolved NH4, NO3, and FRP.
    These are either a single lake-wide constant, a per-benthic-zone
    constant (`&aed_sed_const2d`), or output from a full
    early-diagenesis model (`dynamic`/`dynamic2d`, not covered in detail
    here).
2.  **Physical resuspension** (`aed_noncohesive.F90`) — a
    critical-shear-stress erosion law computed **once**, whose mass flux
    (`NCS_resus`) is then reused by `aed_phosphorus.F90` (adsorbed P)
    and `aed_organic_matter.F90` (particulate organic N/P) via fixed
    stoichiometric ratios, rather than each module computing its own
    erosion physics.

## The two parameterisation approaches

### Approach A — Prescribed flux, unlimited supply (the default)

`simSedimentMass = .false.` (default, `aed_noncohesive`) and
`simSedimentOM = 0` (default, `aed_organic_matter`). No sediment or
sediment-nutrient inventory is tracked at all. Resuspension flux is a
pure function of hydrodynamic forcing and fixed erosion parameters
(`epsilon`, `tau_0`, `tau_r`); diffusive fluxes are prescribed rates.
Neither can ever be depleted, by construction.

### Approach B — Finite pool, mass-tracked

`simSedimentMass = .true.` and/or `simSedimentOM = 1` or `2`. A real
benthic sheet state variable is defined per pool (`*_sed` for inorganic
sediment, `sed_poc`/`sed_pon`/`sed_pop` for organic matter), initialised
from `sedimentBulkDens × sedimentDepth × sedimentOMfrac` (and the
`Xsn`/`Xsp` ratios), and debited by resuspension / credited by settling
each step.

## The pool does not feed back on the flux

This is the critical, non-obvious detail: in both modules, the erosion
flux calculation **never reads the current pool size**. Turning mass
tracking on gives you *bookkeeping*, not a *constraint*. If erosion
parameters chronically outpace deposition, the inorganic pool clips at
its hard floor (`minimum = zero_`) — producing a flux discontinuity —
while the organic pools have essentially no floor at all
(`minimum = -1e10`,
[`aed_organic_matter.F90:459`](https://github.com/AquaticEcoDynamics/libaed-water/blob/9498c5b7390d8e72e1eea32e1bfee0c8d06be84e/src/aed_organic_matter.F90#L459))
and can drift into deeply negative, physically meaningless territory
**silently, with no warning or error**.

## Trade-off summary

|  | Approach A: Unlimited / prescribed | Approach B: Finite pool, mass-tracked |
|----|----|----|
| **What limits resuspension** | Nothing — pure function of shear stress vs. threshold | Nothing either (see callout above) — but the pool *itself* can run out |
| **Risk of silent failure** | None | Yes — pool clips (inorganic) or goes unphysically negative (organic) with no warning |
| **Represents long-term depletion/enrichment** | No | Yes, if parameterised and validated carefully |
| **Calibration burden** | Lower — matches how most Fsed\_\* values are calibrated against observed data (assumes persistent supply) | Higher — needs a realistic initial mass (ideally from sediment core data) and erosion/deposition parameters that are mass-balanced over the *entire* run length, not just a short spin-up window |
| **Validation needed before trusting a 100-yr run** | Confirm water-column `NCS_ss` reaches dynamic quasi-equilibrium (fast — settling velocity / flushing time, typically weeks–months) | Confirm the `sed_*` diagnostic time series does **not** approach its floor at any point across the full run |
| **Suitable research questions** | Stratification, event-driven turbidity/nutrient pulses, general water quality where sediment supply isn’t the question | Studies specifically about whether sediment nutrient *stocks* deplete or accumulate under changing disturbance regimes (e.g. increased storminess over decades) |

## `aed.nml` parameterisation

### Approach A: unlimited supply

No special settings are required beyond the normal resuspension physics
— this is the default behaviour if `simSedimentMass`/`simSedimentOM` are
left unset.

``` ini
&aed_noncohesive
   num_ss = 1
   settling = 1
   w_ss = -0.01
   rho_ss = 1.6e3
   d_ss = 4e-6
   resuspension = 1        ! 0=off, 1=static tau_0, 2/3=macrophyte-modified
   epsilon = 0.02           ! max erosion rate, g/m2/s
   tau_0 = 0.04              ! critical shear stress, N/m2
   tau_r = 1.0                ! reference shear stress for normalisation
   ! simSedimentMass left unset -> defaults to .false. (unlimited)
   sed_porosity = 0.3
/

&aed_phosphorus
   ...
   simPO4Adsorption = .true.
   resuspension = 1
   resus_link = 'NCS_resus'   ! reuse the noncohesive erosion flux
/

&aed_organic_matter
   ...
   resuspension = 1
   resus_link = 'NCS_resus'
   Xsn = 0.005                ! N:sediment mass ratio for resuspended organic N
   Xsp = 0.001                ! P:sediment mass ratio for resuspended organic P
   ! simSedimentOM left unset -> defaults to 0 (unlimited)
/
```

Diffusive fluxes (independent of resuspension, always unlimited
regardless of approach) are set directly, e.g.:

``` ini
&aed_sedflux
   sedflux_model = 'Constant2d'
/
&aed_sed_const2d
   n_zones = 2
   active_zones = 1, 2
   fsed_amm = 3.4, 0.7
   fsed_nit = -0.4, 0.1
   fsed_frp = 0.04, 0.06
/
```

### Approach B: finite, mass-tracked pool

Add the mass-tracking flags, size the initial inventory, and expose the
diagnostics needed to monitor it:

``` ini
&aed_noncohesive
   ! ... same erosion parameters as above, plus:
   simSedimentMass = .true.
   ss_initial = 0.0           ! water-column initial concentration
   sed_porosity = 0.3
   diag_level = 5              ! must be > 0 to expose *_sed diagnostics
/

&aed_organic_matter
   ! ... same resuspension linkage as above, plus:
   simSedimentOM = 2           ! 1 = start pools at zero; 2 = initialise from bulk density/%OM
   sedimentBulkDens = 1.3e3    ! kg/m3 - use site sediment core data if available
   sedimentDepth = 0.5          ! m - the "active" sediment layer depth being tracked
   sedimentOMfrac = 0.05        ! fraction of that layer that's organic matter
   Xsn = 0.005
   Xsp = 0.001
/
```

## Before trusting a 100-year Approach-B run

1.  Run the full intended simulation length (or a representative long
    segment) once as a validation pass.
2.  Plot the `sed_ss`, `sed_poc`, `sed_pon`, `sed_pop` diagnostics over
    the whole run.
3.  If any approaches zero (inorganic) or drifts substantially negative
    (organic), the parameterisation is not mass-balanced for this
    scenario. A longer spin-up will not fix this — it only reaches the
    same outcome sooner. Instead, revisit `sedimentDepth`/initial mass
    (ideally informed by measured sediment core data) or reconcile
    `epsilon`/`tau_0` against realistic long-term deposition rates.

## Spin-up guidance

| Configuration | What needs to equilibrate | Typical timescale |
|----|----|----|
| Approach A (any) | Water-column `NCS_ss` reaching dynamic balance between resuspension input and settling + outflow loss | Weeks–months (settling velocity / lake flushing time) — short relative to a century-scale run, but confirm before treating early events as representative |
| Approach B | Whether the finite pool is mass-balanced *at all* over the run length (see callout above) | Not a spin-up in the traditional sense — a mandatory full-length validation pass |
| Macrophyte-stabilised resuspension (`resuspension = 2` or `3`) | `tau_0` itself depends on macrophyte biomass, a slow state variable | Years — realistic vegetation community establishment, independent of the sediment-pool question above |

## Recommendation for long-term climate simulations

**Default to Approach A (prescribed/unlimited flux)** unless the
specific research question is about sediment nutrient stock depletion or
accumulation itself. Reasons:

- It matches how `Fsed_*` values are conventionally calibrated (against
  observed flux data, implicitly assuming persistent supply), so it’s
  the better-validated, more standard configuration.
- It carries no risk of the silent, undetected failure mode described
  above — a real concern over a 100-year run where an early-onset pool
  depletion partway through could invalidate the back half of the
  simulation without any error being raised.
- Its spin-up requirement (water-column suspended sediment reaching
  dynamic equilibrium) is short and easy to verify relative to the full
  run length.

**Use Approach B deliberately, not by default**, when the research
question specifically requires tracking whether sediment nutrient stocks
change under the climate scenario (e.g., “does increased storm frequency
exhaust erodible sediment P over the century?”). In that case, treat the
mass-balance validation in the warning box above as mandatory before
interpreting results, size the initial pool from real sediment core data
where possible, and consider reporting the depletion trajectory itself
as a result rather than a nuisance to be spun away.

A middle-ground option worth considering: run with Approach A as the
primary configuration (for robustness), but also enable the Approach B
diagnostics (`simSedimentMass = .true.`, `diag_level > 0`) purely as a
**monitoring overlay** — i.e., let the flux behave as unlimited, but
still track what the implied cumulative depletion *would* be, and treat
any point where that tracked figure would go negative as a flag that
resuspension is behaving unrealistically for that period of the
scenario.

## Source references

All links are pinned to `libaed-water` commit
[`9498c5b`](https://github.com/AquaticEcoDynamics/libaed-water/commit/9498c5b7390d8e72e1eea32e1bfee0c8d06be84e)
(2026-08-01), the `master`-branch HEAD at the time this note was last
checked against the source. Re-verify if `libaed-water` has moved on
significantly since.

- Sediment flux hub:
  [`aed_sedflux.F90`](https://github.com/AquaticEcoDynamics/libaed-water/blob/9498c5b7390d8e72e1eea32e1bfee0c8d06be84e/src/aed_sedflux.F90)
- Nitrogen benthic flux:
  [`aed_nitrogen.F90`](https://github.com/AquaticEcoDynamics/libaed-water/blob/9498c5b7390d8e72e1eea32e1bfee0c8d06be84e/src/aed_nitrogen.F90),
  [`aed_calculate_benthic_nitrogen`](https://github.com/AquaticEcoDynamics/libaed-water/blob/9498c5b7390d8e72e1eea32e1bfee0c8d06be84e/src/aed_nitrogen.F90#L679)
- Phosphorus benthic flux and adsorption:
  [`aed_phosphorus.F90`](https://github.com/AquaticEcoDynamics/libaed-water/blob/9498c5b7390d8e72e1eea32e1bfee0c8d06be84e/src/aed_phosphorus.F90),
  [`aed_calculate_benthic_phosphorus`](https://github.com/AquaticEcoDynamics/libaed-water/blob/9498c5b7390d8e72e1eea32e1bfee0c8d06be84e/src/aed_phosphorus.F90#L447)
- Resuspension erosion law:
  [`aed_noncohesive.F90`](https://github.com/AquaticEcoDynamics/libaed-water/blob/9498c5b7390d8e72e1eea32e1bfee0c8d06be84e/src/aed_noncohesive.F90),
  [`aed_calculate_benthic_noncohesive`](https://github.com/AquaticEcoDynamics/libaed-water/blob/9498c5b7390d8e72e1eea32e1bfee0c8d06be84e/src/aed_noncohesive.F90#L348)
- Particulate organic N/P benthic pools:
  [`aed_organic_matter.F90`](https://github.com/AquaticEcoDynamics/libaed-water/blob/9498c5b7390d8e72e1eea32e1bfee0c8d06be84e/src/aed_organic_matter.F90),
  [`aed_calculate_benthic_organic_matter`](https://github.com/AquaticEcoDynamics/libaed-water/blob/9498c5b7390d8e72e1eea32e1bfee0c8d06be84e/src/aed_organic_matter.F90#L1054)
- Repository:
  [`AquaticEcoDynamics/libaed-water`](https://github.com/AquaticEcoDynamics/libaed-water)
  (branch `master`, as used by GLM `v4alpha`/`master` via
  `build_aedlibs.inc`)
