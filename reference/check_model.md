# Check model name and return standardized code

Check model name and return standardized code

## Usage

``` r
check_model(model, os_valid = FALSE)
```

## Arguments

- model:

  Character vector of model names. Valid options are: "DYRESM-CAEDYM",
  "GLM-AED", "GOTM-WET", "SIMSTRAT-AED2" or their corresponding codes
  "dy_cd", "glm_aed", "gotm_wet", "simstrat_aed2".

- os_valid:

  Logical. If TRUE, checks if the model is valid for the current
  operating system.

## Value

Character vector of standardized model codes.

## Examples

``` r
check_model(c("GLM-AED", "gotm_wet"))
#>    GLM-AED   GOTM-WET 
#>  "glm_aed" "gotm_wet" 
```
