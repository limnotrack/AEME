# Check model output

Check model output

## Usage

``` r
check_model_output(aeme, model, path)
```

## Arguments

- aeme:

  aeme; object.

- model:

  vector; of models to be used. Can be `dy_cd`, `glm_aed`, `gotm_wet`.

- path:

  filepath; where input files are located relative to the current
  working directory.

## Value

Invisibly TRUE if model output passes checks; otherwise aborts
