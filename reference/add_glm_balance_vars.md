# Add variables to the GLM-AED mass balance

Adds a group of variables, or individual state variables, to the
`&mass_balance` block of an existing `glm_aed` configuration, keeping
the variables already there. Variables already in the list are not
repeated.

## Usage

``` r
add_glm_balance_vars(aeme, groups = NULL, vars = NULL, path = NULL)
```

## Arguments

- aeme:

  Aeme object.

- groups:

  character; groups to add, any of `"TN"`, `"TP"`, `"TOC"`, `"TSS"` and
  `"other"`, as in
  [`set_glm_balance_groups()`](https://limnotrack.com/AEME/reference/set_glm_balance_groups.md).
  `NULL` adds no group.

- vars:

  character; individual AED state variables to add, such as `"OXY_oxy"`
  or `"PHY_cyano"`. They must be listed in the `wq_names` of the
  configuration.

- path:

  character; directory where input files are located. Defaults to the
  path stored in `aeme`, or the current working directory if not set.

## Value

`aeme` with the `&mass_balance` block of the `glm_aed` configuration
updated. Use
[`write_configuration()`](https://limnotrack.com/AEME/reference/write_configuration.md)
to write it to file.

## See also

[`set_glm_balance_groups()`](https://limnotrack.com/AEME/reference/set_glm_balance_groups.md)

## Examples

``` r
if (FALSE) { # \dontrun{
aeme <- add_glm_balance_vars(aeme, groups = "other")
aeme <- add_glm_balance_vars(aeme, vars = c("OXY_oxy", "SIL_rsi"))
} # }
```
