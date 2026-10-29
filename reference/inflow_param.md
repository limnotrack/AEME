# Parameters for scaling or setting inflow variables

Builds parameter rows that change an inflow variable such as DOC during
calibration. A multiplier (`file = "inf"`) scales the variable in every
inflow that has it; a constant (`file = "inf_const"`) replaces it with a
single value. Flow is scaled with the existing `inflow` parameter in
[aeme_parameters](https://limnotrack.com/AEME/reference/aeme_parameters.md).

## Usage

``` r
inflow_param(
  name,
  value = 1,
  min = 0.5,
  max = 2,
  model = c("dy_cd", "glm_aed", "gotm_wet", "simstrat_aed2", "simstrat_aed"),
  constant = FALSE
)
```

## Arguments

- name:

  character; inflow variable(s), e.g. `"CAR_doc"`.

- value:

  numeric; starting value (multiplier or constant).

- min, max:

  numeric; calibration bounds.

- model:

  character; model(s) to create rows for. Default is all five models.

- constant:

  logical; `TRUE` for constants (`inf_const`), `FALSE` (default) for
  multipliers (`inf`).

## Value

data.frame with the columns of
[aeme_parameters](https://limnotrack.com/AEME/reference/aeme_parameters.md)
that
[`input_model_parameters()`](https://limnotrack.com/AEME/reference/input_model_parameters.md)
requires, to row-bind onto a parameter table.

## Details

Scaling the flow also scales the load of every variable, but leaves its
concentration unchanged. These rows change the concentration
independently. Constants are in the units of the inflow data held in the
Aeme object (e.g. g/m3 for `CAR_doc`); unit conversions to each model
happen afterwards.

## See also

[`input_model_parameters()`](https://limnotrack.com/AEME/reference/input_model_parameters.md),
[`zone_ratio_param()`](https://limnotrack.com/AEME/reference/zone_ratio_param.md)

## Examples

``` r
# Scale DOC concentrations in the inflows by 0.5 to 2
inflow_param("CAR_doc", value = 1, min = 0.5, max = 2, model = "glm_aed")
#> # A tibble: 1 × 8
#>   model   file  name    value   min   max group index
#>   <chr>   <chr> <chr>   <dbl> <dbl> <dbl> <chr> <dbl>
#> 1 glm_aed inf   CAR_doc     1   0.5     2 NA       NA

# Try a constant DOC concentration of 5 g/m3
inflow_param("CAR_doc", value = 5, min = 1, max = 15, model = "glm_aed",
             constant = TRUE)
#> # A tibble: 1 × 8
#>   model   file      name    value   min   max group index
#>   <chr>   <chr>     <chr>   <dbl> <dbl> <dbl> <chr> <dbl>
#> 1 glm_aed inf_const CAR_doc     5     1    15 NA       NA

if (FALSE) { # \dontrun{
# Run GLM-AED with the DOC in the inflows doubled and compare with the
# unscaled run. Inflow DOC only affects models that use inflow
# biogeochemistry (GLM-AED); GOTM-WET reads flow, temperature and salinity.
aeme_file <- system.file("extdata", "aeme.rds", package = "AEME")
aeme <- readRDS(aeme_file)
path <- tempdir()
model_controls <- get_model_controls(use_bgc = TRUE)
aeme <- build_aeme(path = path, aeme = aeme, model = "glm_aed",
                   model_controls = model_controls, ext_elev = 3,
                   use_bgc = TRUE)
base <- run_aeme(aeme, model = "glm_aed", path = path)

# Write the scaled inflow files into the model directory and run again
p <- inflow_param("CAR_doc", value = 2, min = 0.5, max = 4,
                  model = "glm_aed")
aeme <- input_model_parameters(aeme, model = "glm_aed", param = p,
                               path = path)
scaled <- run_aeme(aeme, model = "glm_aed", path = path)

# Surface DOC for both runs
doc <- rbind(
  transform(get_var(base, "glm_aed", "CHM_doc", depth = 0), run = "base"),
  transform(get_var(scaled, "glm_aed", "CHM_doc", depth = 0),
            run = "DOC x2")
)
boxplot(value ~ run, data = doc, ylab = "Surface DOC")
} # }
```
