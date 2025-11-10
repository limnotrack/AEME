# Check model name and return standardized code

Check model name and return standardized code

## Usage

``` r
check_model(model)
```

## Arguments

- model:

  Character vector of model names. Valid options are: "DYRESM-CAEDYM",
  "GLM-AED", "GOTM-WET" or their corresponding codes "dy_cd", "glm_aed",
  "gotm_wet".

  @importFrom cli cli_abort

## Value

Character vector of standardized model codes.

## Examples

``` r
check_model(c("GLM-AED", "gotm_wet"))
#> [1] "glm_aed"  "gotm_wet"
```
