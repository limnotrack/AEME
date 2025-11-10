# Toggle between model display names and codes

A helper function to switch between user-friendly model names and their
corresponding codes used in the backend.

## Usage

``` r
toggle_models(model = NULL, to = NULL)
```

## Arguments

- model:

  Model name to be toggled.

- to:

  Target format: either "display" for user-friendly names or "code" for
  backend codes. If NULL, the function will toggle to the opposite
  format.

## Value

The model name in the desired format.

## Examples

``` r
toggle_models("DYRESM-CAEDYM")  # Returns "dy_cd"
#> DYRESM-CAEDYM 
#>       "dy_cd" 
toggle_models("dy_cd", to = "display")  # Returns "DYRESM-CAEDYM"
#> [1] "DYRESM-CAEDYM"
toggle_models("GLM-AED")  # Returns "glm_aed"
#>   GLM-AED 
#> "glm_aed" 
toggle_models("gotm_wet", to = "display")  # Returns "GOTM-WET"
#> [1] "GOTM-WET"
toggle_models("GOTM-WET", to = "code")  # Returns "gotm_wet"
#>   GOTM-WET 
#> "gotm_wet" 
df <- data.frame(model = rep(c("dy_cd", "glm_aed"), 50))
df <- df |>
        dplyr::mutate(Model = toggle_models(model, to = "display"))
```
