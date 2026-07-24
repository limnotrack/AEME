# Estimate zone-specific sediment fluxes from hypsograph

Estimates zone-specific sediment fluxes for `aed_sed_const2d` using up
to two tiers of adjustment:

## Usage

``` r
estimate_zone_fluxes(
  aeme,
  path,
  ref_depth = 5,
  baseline = c(fsed_oxy = -25, fsed_amm = 2, fsed_nit = 0.2, fsed_frp = 0.05),
  verbose = TRUE
)
```

## Arguments

- aeme:

  Aeme object.

- path:

  character; directory where input files are located. Defaults to the
  path stored in `aeme`, or the current working directory if not set.

- ref_depth:

  Numeric. Reference depth (m) for literature baseline fluxes. Default
  `5`.

- baseline:

  Named numeric vector of baseline fluxes at `ref_depth`. Must include
  `fsed_oxy`, `fsed_amm`, `fsed_nit`, `fsed_frp`.

- verbose:

  Logical. Print zone summary and copy-paste config lines. Default
  `TRUE`.

## Value

Invisibly returns a named list with the following elements:

- `fsed_oxy`:

  Numeric vector of length `n_zones`. Sediment oxygen demand flux (mmol
  O2/m2/d, negative).

- `fsed_amm`:

  Numeric vector of length `n_zones`. Ammonium flux (mmol N/m2/d).

- `fsed_nit`:

  Numeric vector of length `n_zones`. Nitrate flux (mmol N/m2/d).

- `fsed_frp`:

  Numeric vector of length `n_zones`. Filterable reactive phosphorus
  flux (mmol P/m2/d).

- `zone_summary`:

  Data frame of zone geometry and final flux estimates.

- `method`:

  Character string; either `"baseline_scaled"` or `"obs_adjusted"`.

## Details

**Tier 1 (always)** – area-weighted depth scaling. Each zone's flux is
scaled from literature baseline values according to its mean depth and
fractional bed area. Deep zones receive higher SOD and nutrient fluxes
reflecting greater organic matter accumulation and more persistent
anoxia.

**Tier 2 (optional, when `obs` supplied)** – observed data adjustment.
Near-bed summer concentrations of O2, NH4, NO3, and FRP are used to
adjust the relative difference in fluxes between zones. Only inter-zone
ratios are adjusted, not absolute magnitude, so the lake-wide total is
preserved.

Literature baselines at reference depth 5 m (temperate lakes):

- `fsed_oxy`: -25 mmol O2/m2/d (Muller et al. 2012; Sondergaard et al.
  2003)

- `fsed_amm`: 2 mmol N/m2/d (Andersen 1982; Beutel 2006)

- `fsed_nit`: 0.2 mmol N/m2/d (Seitzinger 1988)

- `fsed_frp`: 0.05 mmol P/m2/d (Nurnberg 1984)

Depth scaling (Beutel 2006; Muller et al. 2012): SOD and NH4/FRP fluxes
scale approximately linearly with mean zone depth divided by
`ref_depth`. NO3 flux transitions from small positive values (shallow,
oxic) to negative values (deep, anoxic denitrification) at approximately
`0.5 * max_depth`.

## References

Beutel, M.W. (2006).
[doi:10.1016/j.ecoleng.2006.05.009](https://doi.org/10.1016/j.ecoleng.2006.05.009)

Muller, B., et al. (2012).
[doi:10.1021/es301422r](https://doi.org/10.1021/es301422r)

Nurnberg, G.K. (1984).
[doi:10.4319/lo.1984.29.1.0111](https://doi.org/10.4319/lo.1984.29.1.0111)

Seitzinger, S.P. (1988).
[doi:10.4319/lo.1988.33.4part2.0702](https://doi.org/10.4319/lo.1988.33.4part2.0702)

Sondergaard, M., et al. (2003).
[doi:10.1023/B:HYDR.0000008611.12704.dd](https://doi.org/10.1023/B%3AHYDR.0000008611.12704.dd)

## Examples

``` r
if (FALSE) { # \dontrun{
zone_heights <- estimate_sed_zones(hypsograph)

# Tier 1 only
fluxes <- estimate_zone_fluxes(zone_heights, hypsograph)

# Tier 2 with observations (Southern Hemisphere)
fluxes <- estimate_zone_fluxes(
  zone_heights,
  hypsograph,
  obs = obs_df,
  lat = -38
)
} # }
```
