# Match variable to AEME variable names

This function takes a character vector of variable names and checks them
against the official AEME variable names. If an input variable does not
match any official name, the function attempts to find the closest match
using both exact keyword matching and fuzzy string matching (Levenshtein
distance). The function returns a character vector of the same length as
the input, where each element is either the original variable name (if
it was valid) or the best-matching official AEME variable name. If no
suitable match is found for an invalid variable, a warning is issued.

## Usage

``` r
guess_aeme_vars(x, key_filter)
```

## Arguments

- x:

  Character vector of variable names to check.

- key_filter:

  Optional string to filter the AEME variable names by a specific
  keyword. If provided, only AEME variable names containing this keyword
  will be considered for matching. This can help improve matching
  accuracy by limiting the pool of candidate variable names to those
  relevant to a particular context (e.g., "met" for meteorological
  variables).

## Value

Character vector of variable names, with invalid names replaced by the
closest official AEME variable name where possible.

## Examples

``` r
guess_aeme_vars(c("temp", "oxy", "ph", "chla", "tp", "tn"))
#> ✔ Variable 'temp' matched to 'HYD_temp'.
#> ✔ Variable 'oxy' matched to 'CHM_oxy'.
#> ✔ Variable 'ph' matched to 'CHM_ph'.
#> ✔ Variable 'chla' matched to 'PHY_tchla'.
#> ✔ Variable 'tp' matched to 'PHS_tp'.
#> ✔ Variable 'tn' matched to 'NIT_tn'.
#> [1] "HYD_temp"  "CHM_oxy"   "CHM_ph"    "PHY_tchla" "PHS_tp"    "NIT_tn"   
guess_aeme_vars(c("temp", "swr", "lwr", "wind", "precip"), key_filter = "MET")
#> ✔ Variable 'temp' matched to 'MET_tmpair'.
#> ✔ Variable 'swr' matched to 'MET_radswd'.
#> ✔ Variable 'lwr' matched to 'MET_radlwd'.
#> ✔ Variable 'wind' matched to 'MET_wndspd'.
#> ✔ Variable 'precip' matched to 'MET_pprain'.
#> [1] "MET_tmpair" "MET_radswd" "MET_radlwd" "MET_wndspd" "MET_pprain"
```
