# Choose the GLM-AED mass balance variable groups

GLM accepts at most 15 `balance_vars` in the `&mass_balance` block, so
AEME reports only the nitrogen, phosphorus and suspended-solids totals'
components by default. This sets which groups are reported for an
existing configuration, based on the AED configuration (the `aed_totals`
components for the phytoplankton groups and AED modules that are
active).

## Usage

``` r
set_glm_balance_groups(aeme, groups = c("TN", "TP", "TSS"), path = NULL)
```

## Arguments

- aeme:

  Aeme object.

- groups:

  character; any of `"TN"`, `"TP"`, `"TOC"` (components of the total
  nitrogen, phosphorus and organic carbon), `"TSS"` (suspended solids)
  and `"other"` (remaining active state variables such as oxygen and
  silica). Variables are taken in this order and truncated at 15 with a
  warning.

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
`options(AEME.glm_balance_groups = c("TN", "TP", "TOC"))`.

## Examples

``` r
if (FALSE) { # \dontrun{
aeme <- set_glm_balance_groups(aeme, groups = c("TN", "TOC"))
} # }
```
