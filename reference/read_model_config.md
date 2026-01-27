# Read in model configuration files for a given model and lake directory

Read in model configuration files for a given model and lake directory

## Usage

``` r
read_model_config(model, lake_dir)
```

## Arguments

- model:

  character; model name ("dy_cd", "glm_aed", "gotm_wet"). Only one model
  at a time.

- path:

  character; directory which contains the model configuration files.

## Value

List with model configuration components. This includes a 'hydrodynamic'
list with hydrodynamic model configuration and a 'bgc' list with
biogeochemistry model configuration (if applicable).
