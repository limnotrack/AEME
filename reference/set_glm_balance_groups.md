# Choose the GLM-AED mass balance variable groups

Sets which variables the GLM `&mass_balance` block reports for an
existing configuration, based on the AED configuration (the `aed_totals`
components for the phytoplankton groups and AED modules that are
active). By default all components of the nitrogen, phosphorus, organic
carbon and suspended solids totals are reported. This replaces the
current list; use
[`add_glm_balance_vars()`](https://limnotrack.com/AEME/reference/add_glm_balance_vars.md)
to add to it.

## Usage

``` r
set_glm_balance_groups(aeme, groups = .glm_balance_groups_default, path = NULL)
```

## Arguments

- aeme:

  Aeme object.

- groups:

  character; any of `"TN"`, `"TP"`, `"TOC"` (components of the total
  nitrogen, phosphorus and organic carbon), `"TSS"` (suspended solids)
  and `"other"` (remaining active state variables such as oxygen and
  silica). Variables are taken in this order.

- path:

  character; directory where input files are located. Defaults to the
  path stored in `aeme`, or the current working directory if not set.

## Value

`aeme` with the `&mass_balance` block of the `glm_aed` configuration
updated. Use
[`write_configuration()`](https://limnotrack.com/AEME/reference/write_configuration.md)
to write it to file.

## Details

The default for new builds can be changed with
`options(AEME.glm_balance_groups = c("TN", "TP"))`.

GLM builds before 4.0.0+ accept at most 15 balance variables and stop
with an error if more are given; with such a build choose fewer
`groups`.

## See also

[`add_glm_balance_vars()`](https://limnotrack.com/AEME/reference/add_glm_balance_vars.md)

## Examples

``` r
if (FALSE) { # \dontrun{
aeme <- set_glm_balance_groups(aeme, groups = c("TN", "TOC"))
} # }
```
