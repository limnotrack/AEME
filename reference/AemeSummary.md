# S4 class for a lightweight summary of an Aeme object

An `AemeSummary` holds what a dashboard or report needs to show the
result of an AEME run (surface/bottom time series per model, the
observations and forcing that went with them, the hypsograph) without
the depth-resolved model output or the model configuration. It is
created by
[`summarise_aeme()`](https://limnotrack.com/AEME/reference/summarise_aeme.md).
It is deliberately **not** an
[Aeme](https://limnotrack.com/AEME/reference/Aeme.md): it cannot be
built or run.

## Usage

``` r
# S4 method for class 'AemeSummary'
names(x)
```

## Arguments

- x:

  An `AemeSummary` object.

## Slots

- `lake`:

  list; as the `lake` slot of the source `Aeme`.

- `time`:

  list; `start`, `stop`, `tz` and `output_time_step`.

- `models`:

  character; the models that were summarised.

- `meta`:

  list; `aeme_version` (of the package that made the summary),
  `source_version` (of the source object), `created`, `n_members`,
  `period`, `layer_rule` and `stats`.

- `output`:

  data.frame; long format with columns `Date`, `model`, `var_aeme`,
  `layer`, `stat` and `value`. `layer` is `"surface"`, `"bottom"` or
  `NA` (variables without a depth dimension). `stat` is `"value"` for a
  single run, or the ensemble statistics (e.g. `"mean"`, `"min"`,
  `"max"`, `"q05"`, `"q95"`), plus the member labels (`"ens_001"`, ...)
  when members are kept.

- `profiles`:

  data.frame; seasonal mean profiles (`season`, `model`, `var_aeme`,
  `depth`, `mean`, `sd`, `n`). Zero rows unless requested.

- `observations`:

  list; `lake` and `level` observation data frames, as in an `Aeme`,
  where present.

- `forcing`:

  list; daily long tables `meteo` (`Date`, `var_aeme`, `value`),
  `inflows` and `outflows` (`Date`, `name`, `var_aeme`, `value`) and
  `water_balance` (`Date`, `source`, `var_aeme`, `value`), where
  present.

- `hypsograph`:

  data.frame; the hypsograph, unchanged.

- `parameters`:

  data.frame; the model parameters, unchanged.

## See also

[`summarise_aeme()`](https://limnotrack.com/AEME/reference/summarise_aeme.md),
[`write_aeme_summary()`](https://limnotrack.com/AEME/reference/write_aeme_summary.md),
[`read_aeme_summary()`](https://limnotrack.com/AEME/reference/write_aeme_summary.md)
