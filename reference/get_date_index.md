# Get date index for each model in the AEME object

Get date index for each model in the AEME object

## Usage

``` r
get_date_index(
  aeme,
  model,
  remove_spin_up = TRUE,
  path = NULL,
  lake_dir = NULL,
  daily_mean = FALSE
)
```

## Arguments

- aeme:

  Aeme object.

- model:

  character vector; models to use. One or more of `"dy_cd"`,
  `"glm_aed"`, `"gotm_wet"`, `"simstrat_aed2"`. Defaults to all models
  if not found in `aeme`.

- remove_spin_up:

  logical; remove spin-up period from plot. Default is TRUE.

- path, lake_dir:

  optional; root path or resolved lake directory used to locate each
  model's output file. When supplied (and the file exists), the
  reconstructed positional index is trimmed to the number of records the
  file actually holds, so a stale cadence (e.g. an hourly
  `output_time_step` against a run still written daily) is caught here
  rather than silently emptying the output downstream. When omitted, the
  index is the pure `aeme_time_axis()` reconstruction, exactly as
  before.

## Value

A list with date index for each model
