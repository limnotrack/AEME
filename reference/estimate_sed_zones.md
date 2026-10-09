# Estimate sediment zones based on hypsograph

Estimate sediment zones based on hypsograph

## Usage

``` r
estimate_sed_zones(hypsograph, n_zones = NULL)
```

## Arguments

- hypsograph:

  data frame with columns "depth" and "area". Depth should be negative
  below the surface and positive above. Area should be the lake area at
  each depth. Hypsograph should be ordered by depth (descending).

- n_zones:

  integer or `NULL`; number of sediment zones to return. `NULL`
  (default) lets the zone count be inferred from the hypsograph slope
  (`estimate_n_zones_hyps()`). When given, exactly `n_zones` heights are
  returned, split by cumulative benthic area (falling back to evenly
  spaced heights if the hypsograph is too coarse for that many zones).

## Value

A numeric vector with the estimated heights of each sediment zone. The
length of the vector corresponds to the number of zones. The heights are
cumulative from the lake bottom (i.e., the first value is the height of
the first zone from the bottom, the second value is the height of the
second zone from the bottom, etc.). The last value should be equal to
the maximum depth of the lake.
