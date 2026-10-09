# Suggest a light extinction (Kw) regime for GLM

Diagnoses a light extinction coefficient (Kw, i.e. Kd, m^-1) or Secchi
depth time series and recommends how to parameterise light extinction
for a GLM(-AED) run: a single static Kw, a monthly Kw climatology, or a
fully dated time-varying Kw forcing.

## Usage

``` r
suggest_kw_regime(
  date,
  kw = NULL,
  secchi = NULL,
  secchi_coef = 1.7,
  min_n = 12,
  min_years = 3,
  max_median_gap_days = 60,
  interannual_r2_threshold = 0.3,
  seasonal_ratio_threshold = 1.3
)
```

## Arguments

- date:

  Date vector (or coercible via `as.Date`), one per observation.

- kw:

  Numeric vector of light extinction coefficients (Kd, m^-1). Supply
  either `kw` or `secchi`, not both.

- secchi:

  Numeric vector of Secchi depths (m). Converted to Kd via
  `Kd = secchi_coef / secchi`.

- secchi_coef:

  Coefficient used to convert Secchi depth to Kd (default 1.7, the
  commonly used Poole & Atkins mid-range value). Override if a
  site-specific Secchi:Kd relationship is known.

- min_n:

  Minimum number of observations required before any seasonal or
  interannual pattern is considered resolvable (default `12`).

- min_years:

  Minimum span of distinct years required to assess interannual
  variability (default `3`).

- max_median_gap_days:

  Maximum acceptable median sampling gap (days) for a monthly
  climatology to be considered resolvable (default `60`).

- interannual_r2_threshold:

  Fraction of total variance explained by year, above which interannual
  variability is judged to dominate over any recurring seasonal cycle
  (default `0.30`).

- seasonal_ratio_threshold:

  Ratio of max:min monthly mean (after removing each year's own mean)
  above which a recurring seasonal cycle is judged strong enough to
  justify a monthly climatology (default `1.3`, i.e. a \>30% swing).

## Value

An object of class `kw_regime` (a list) with the recommendation,
supporting diagnostics, and (where relevant) a monthly Kw climatology
data frame ready to use as GLM forcing.

## Checks performed

- **sufficient_n**: are there enough observations (`>= min_n`) to
  resolve any seasonal or interannual pattern.

- **sufficient_years**: does the record span enough distinct years
  (`>= min_years`) to separate a recurring seasonal cycle from
  interannual variability.

- **sufficient_monthly_resolution**: is the median sampling gap (days)
  small enough (`<= max_median_gap_days`) to support a monthly
  climatology.

- **interannual_dominant**: does year explain more of the total variance
  in Kw (`>= interannual_r2_threshold`) than a recurring seasonal cycle
  would.

- **seasonal_signal_present**: is there a real recurring seasonal cycle
  – the detrended (year-mean-removed) ratio of max:min monthly mean is
  `>= seasonal_ratio_threshold`.

Based on these, the function recommends one of `"static"`,
`"static_or_single_year_monthly"`, `"dated_timeseries"` or
`"monthly_climatology"`.

## See also

[`plot.kw_regime()`](https://limnotrack.com/reference/plot.kw_regime.md),
[`set_glm_param()`](https://limnotrack.com/reference/set_glm_param.md)

## Examples

``` r
set.seed(1)
dates <- seq.Date(as.Date("2018-01-01"), as.Date("2021-12-01"), by = "month")
kw <- 1 + 0.4 * sin(2 * pi * (as.integer(format(dates, "%m")) - 3) / 12) +
  rnorm(length(dates), sd = 0.05)
rec <- suggest_kw_regime(dates, kw = kw)
rec
#> 
#> ── Kw regime suggestion ────────────────────────────────────────────────────────
#> • Input: kw (n = 48, 0 dropped for NA/ordering)
#> • Date range: 2018-01-01 to 2021-12-01 (4 distinct years)
#> • Median sample gap: 31 days (max 31 days)
#> • Kw: mean 1.004, median 1.024, sd 0.299, CV 0.30 (m^-1)
#> • Interannual R^2: 0% of variance explained by year
#> • Seasonal ratio: 2.40x (max:min monthly mean, detrended by year)
#> 
#> ── Recommendation: MONTHLY Kw CLIMATOLOGY (time-varying) ──
#> 
#> Sampling is frequent enough (median gap 31 days) and spans 4 years with a real
#> recurring seasonal cycle (detrended seasonal ratio = 2.40x) that is not swamped
#> by interannual variability (year R^2 = 0%). Recommend a monthly Kw climatology
#> (see `$monthly_climatology`) as a time-varying GLM forcing. If the model run
#> covers the actual sampled period, the dated observed time series is still
#> preferable to the climatology.
#> 
#> ── Monthly climatology ──
#> 
#>  month n      mean         sd
#>      1 4 0.6408129 0.02967646
#>      2 4 0.7731688 0.05622444
#>      3 4 1.0154192 0.04845679
#>      4 4 1.2105346 0.06528734
#>      5 4 1.3422932 0.01676838
#>      6 4 1.4035993 0.03853715
#>      7 4 1.3884638 0.01857737
#>      8 4 1.2223263 0.01872480
#>      9 4 1.0149209 0.03469513
#>     10 4 0.7964431 0.03146693
#>     11 4 0.6607632 0.05942729
#>     12 4 0.5844254 0.06112999
```
