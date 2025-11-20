# Read model outputs and format to AEME standard

Read model outputs and format to AEME standard

## Usage

``` r
read_model_outputs(
  lake_dir,
  model,
  vars_sim = NULL,
  depths = NULL,
  dates = NULL,
  date_index = NULL,
  incl_fluxes = TRUE,
  output_hour = 0
)
```

## Arguments

- lake_dir:

  Directory of lake model outputs

- model:

  Model name. One of "gotm_wet", "glm_aed", or "dy_cd".

- vars_sim:

  Variables to extract in the AEME format e.g. "HYD_temp"

- depths:

  Depths to extract. If NULL, extract all model layer depths. Defaults
  to NULL.

- dates:

  Dates to extract. If NULL, extract all dates. Defaults to NULL.

- date_index:

  Date index to extract. If NULL, extract all dates. Defaults to NULL.

- incl_fluxes:

  Logical indicating whether to include flux variables. Defaults to
  TRUE.

- output_hour:

  Hour of the day to extract (0-23). Defaults to 0.

## Value

List of model outputs in AEME standard format
