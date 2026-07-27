# Load model hypsograph from configuration

Load model hypsograph from configuration

## Usage

``` r
read_model_hypsograph(model, lake_dir, file = NULL)
```

## Arguments

- model:

  Model name. One of "gotm_wet", "glm_aed", or "dy_cd".

- lake_dir:

  Directory of lake model outputs

- file:

  Optional; path directly to the model's hypsograph/ configuration file,
  bypassing the `lake_dir`-based lookup. Defaults to `NULL`.

## Value

Dataframe of hypsograph with columns elev, area, and depth
