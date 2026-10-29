# Read a GLM-AED mass balance file

Reads the GLMv4 `mass_balance.csv` (the `&mass_balance` block) into a
long table. The file has an `Inf`, `Lake` and `Out` column for each
variable in `balance_vars`, in AED native units (mmol).

## Usage

``` r
read_glm_mass_balance(file = NULL, aeme = NULL, path = NULL)
```

## Arguments

- file:

  character; path to `mass_balance.csv`, or to a GLM-AED model directory
  (or `output` directory) containing it. Not needed if `aeme` is given.

- aeme:

  Aeme object. If given (and `file` is `NULL`), the file is found from
  the lake's `glm_aed` output, as in
  [`get_model_outfile()`](https://limnotrack.com/AEME/reference/get_model_outfile.md).

- path:

  character; root directory used with `aeme`. Defaults to the path
  stored in `aeme`.

## Value

a tibble with columns `datetime` (POSIXct, UTC), `var` (GLM-AED variable
name, e.g. `"NIT_nit"`), `term` (`"Inf"`, `"Lake"` or `"Out"`) and
`value` (mmol).

## Examples

``` r
if (FALSE) { # \dontrun{
mb <- read_glm_mass_balance("path/to/glm_aed/output/mass_balance.csv")
} # }
```
