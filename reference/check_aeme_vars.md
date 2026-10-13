# Check AEME variable names

Check if the provided variable names are valid AEME variable names.

## Usage

``` r
check_aeme_vars(x, aeme = NULL)
```

## Arguments

- x:

  Character vector of variable names to check.

- aeme:

  Aeme object, optional. If provided, names that are actually present in
  `aeme`'s loaded output (see
  [`get_output_vars()`](https://limnotrack.com/AEME/reference/get_output_vars.md))
  are also accepted, even without a `key_naming` entry – this covers
  variables
  [`read_glm_output()`](https://limnotrack.com/AEME/reference/read_glm_output.md)
  loaded straight from a GLM/AED output file with no AEME translation
  (`load_all = TRUE`). Such names are passed through as-is, without the
  fuzzy-matching
  [`guess_aeme_vars()`](https://limnotrack.com/AEME/reference/guess_aeme_vars.md)
  would otherwise attempt on them.

  Names in
  [glm_extra_vars](https://limnotrack.com/AEME/reference/glm_extra_vars.md)
  (e.g. `"SED_temp"`) are always accepted.

## Value

Invisibly returns TRUE if all variables are valid, otherwise throws an
error.

## Examples

``` r
check_aeme_vars("HYD_temp")
#> [1] "HYD_temp"
```
