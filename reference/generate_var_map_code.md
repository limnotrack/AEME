# Generate variable mapping code for lake observation data

This function generates R code to create a variable mapping data frame
(\`var_map\`) used in the [`lake_obs_to_aeme()`](lake_obs_to_aeme.md)
function. It takes a data frame of lake observation data and fuzzy
matches variable names to their corresponding AEME variable names using
the \`key_naming\` dataset. When inputting the units of your data,
please ensure they are in the format recognized by the \`units\` package
(e.g. "m", "m^3", "degC", "mg/L", "g/m^3").

## Usage

``` r
generate_var_map_code(data, var_col_name = "name")
```

## Arguments

- data:

  data frame containing lake observations

- var_col_name:

  column name for variable names. If missing, the function will attempt
  to infer it.

## Value

A character string containing R code to create the \`var_map\` data
frame. The code can be copied and pasted into your script and modified
as needed.

## Examples

``` r
#' # Example data frame with lake observation variable names
obs_data <- data.frame(
 name = c("Water Temp (°C)", "pH", "DO (mg/L)", "Chlorophyll-a (µg/L)"),
 value = c(20.5, 7.8, 8.5, 15.2)
)
# Generate variable mapping code
generate_var_map_code(obs_data, var_col_name = "name")
#> Copy and paste the following code to create your variable mapping:
#> var_map <- tibble::tribble(
#>   ~var_aeme, ~name, ~unit,
#>   "CHM_oxy", "DO (mg/L)", "mg/L",
#>   "CHM_ph", "pH", "1",
#>   "HYD_temp", "Water Temp (°C)", "degC",
#>   "PHY_tchla", "Chlorophyll-a (µg/L)", "mg/m^3",
#> )
#> 
#> Update the units to match the units in your data where necessary.
```
