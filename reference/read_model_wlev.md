# Read water level from model output

\`r lifecycle::badge("stable")\` This function reads water level data
from the output of supported lake models.

## Usage

``` r
read_model_wlev(nc = NULL, lake_dir, model)
```

## Arguments

- nc:

  Open netCDF object. If NULL, will open netCDF from lake_dir. This is
  useful when reading multiple variables from the same file to avoid
  reopening the file multiple times. Defaults to NULL.

- lake_dir:

  Directory of lake model outputs

- model:

  Model name. One of "gotm_wet", "glm_aed", or "dy_cd".

## Value

A data frame with columns:

- \`Date\`: Date-time of the water level observation (POSIXct, UTC)

- \`LKE_lvlwtr\`: Water level (meters)
