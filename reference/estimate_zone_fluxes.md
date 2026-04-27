# Estimate zone-specific sediment fluxes from hypsograph

Estimate zone-specific sediment fluxes from hypsograph

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

  aeme; object.

- path:

  filepath; where input files are located relative to the current
  working directory.

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

Invisibly, a named list:

- fsed_oxy:

  Numeric vector, length n_zones (mmol O2/m2/d, negative)

- fsed_amm:

  Numeric vector, length n_zones (mmol N/m2/d)

- fsed_nit:

  Numeric vector, length n_zones (mmol N/m2/d)

- fsed_frp:

  Numeric vector, length n_zones (mmol P/m2/d)

- zone_summary:

  Data frame of zone geometry and final flux estimates

- method:

  Character: "baseline_scaled" or "obs_adjusted"

## References

Beutel (2006) doi:10.1016/j.jhydrol.2006.06.007 Müller et al. (2012)
doi:10.1007/s10750-011-0932-0 Nürnberg (1984)
doi:10.4319/lo.1984.29.1.0111 Seitzinger (1988)
doi:10.4319/lo.1988.33.4part2.0702 Sondergaard et al. (2003)
doi:10.1046/j.1365-2427.2003.01053.x

## Examples

``` r
if (FALSE) { # \dontrun{
zone_heights <- estimate_sed_zones(hypsograph)

# Tier 1 only
fluxes <- estimate_zone_fluxes(zone_heights, hypsograph)

# Tier 2 with observations (Southern Hemisphere)
fluxes <- estimate_zone_fluxes(zone_heights, hypsograph,
                               obs = obs_df, lat = -38)

} # }
```
