# Estimate oxygen saturation concentration

Estimate oxygen saturation concentration

## Usage

``` r
o2_at_sat(
  temp,
  depth,
  baro,
  altitude = 0,
  salinity = rep(0, length(temp)),
  model = "garcia_benson"
)
```

## Arguments

- temp:

  water temperature (°C)

- baro:

  barometric pressure (mb)

- altitude:

  altitude (m). Only used if `baro` is missing.

- salinity:

  salinity (ppt).

- model:

  character, solubility model to use. Options are "garcia",
  "garcia_benson", "weiss", or "benson".

## Value

numeric vector of oxygen saturation concentrations (mg/L)

## References

Colt, John. *1 - Solubility of Atmospheric Gases in Freshwater.* In
Computation of Dissolved Gas Concentration in Water as Functions of
Temperature, Salinity and Pressure (Second Edition), edited by John
Colt, 1-71. London: Elsevier, 2012.
http://www.sciencedirect.com/science/article/pii/B9780124159167000012.

Garcia, H., and L. Gordon (1992), *Oxygen solubility in seawater: Better
fitting equations*, Limnol. Oceanogr., 37(6).

Benson, B. B. & Krause, D. (1984). *The concentration and isotopic
fractionation of oxygen dissolved in freshwater and seawater in
equilibrium with the atmosphere.* Limnology and Oceanography, 29(3),
620-632. doi:10.4319/lo.1984.29.3.0620

Staehr, Peter A., Darren Bade, Matthew C. Van de Bogert, Gregory R.
Koch, Craig Williamson, Paul Hanson, Jonathan J. Cole, and Tim Kratz.
*Lake Metabolism and the Diel Oxygen Technique: State of the Science.*
Limnology and Oceanography: Methods 8, no. 11 (November 1, 2010):
628-44. doi:10.4319/lom.2010.8.0628

USGS. *New Tables of Dissolved Oxygen Saturation Values.* Quality of
Water Branch, 1981. http://water.usgs.gov/admin/memo/QW/qw81.11.html.

USGS. *New Tables of Dissolved Oxygen Saturation Values; Amendment of
Quality of Water Technical Memorandum No. 81.11.* Quality of Water
Branch, 1981. http://water.usgs.gov/admin/memo/QW/qw81.15.html.

USGS. *Change to Solubility Equations for Oxygen in Water.* Technical
Memorandum 2011.03. USGS Office of Water Quality, 2011.

Weiss, R. (1970). *The solubility of nitrogen, oxygen and argon in water
and seawater*. Deep Sea Research and Oceanographic Abstracts, 17(4),
721-735. doi:10.1016/0011-7471(70)90037-9
