# Model configuration with the model parameters applied

`configuration(aeme)` holds each model's configuration as built, before
any model parameters are applied, and `parameters(aeme)` is the table of
updates (e.g. calibrated values) applied on top of it when the model
files are written. `effective_configuration()` combines the two: it
returns the configuration the model files will contain, without changing
`aeme`.

## Usage

``` r
effective_configuration(aeme, model)
```

## Arguments

- aeme:

  Aeme object.

- model:

  character vector; models to return. Defaults to all models in `aeme`.

## Value

A list with the same structure as `configuration(aeme)`, with the
parameters applied to each model's `hydrodynamic` and `bgc` elements.

## Details

Only parameters that set values in a model's configuration files are
applied. Parameters that scale the meteorology, inflows or outflows
(`file` of `"met"`, `"inf"` or `"wdr"`) act on the boundary-condition
data rather than the configuration, so are not reflected here.
Parameters for a file a model's configuration does not have (e.g. bgc
parameters for a model built without bgc) are skipped with a warning.

## See also

[`parameters()`](https://limnotrack.com/reference/parameters.md),
[`configuration()`](https://limnotrack.com/reference/configuration.md),
[`write_configuration()`](https://limnotrack.com/reference/write_configuration.md)

## Examples

``` r
if (FALSE) { # \dontrun{
eff <- effective_configuration(aeme, model = "glm_aed")
eff$glm_aed$hydrodynamic$light$Kw
} # }
```
