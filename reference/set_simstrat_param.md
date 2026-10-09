# Set one or more parameter values in a Simstrat-AED2 `simstrat.par` file

Thin, `aeme`-free wrapper for editing a Simstrat-AED2 `simstrat.par`
(JSON) file in place. Intended for a Simstrat-AED2-only workflow where a
user just wants to tweak parameters, run the model, and load the output.

## Usage

``` r
set_simstrat_param(
  path_simstrat,
  ...,
  par_file = file.path(path_simstrat, "simstrat.par")
)
```

## Arguments

- path_simstrat:

  filepath; directory containing the Simstrat-AED2 configuration

- ...:

  named parameter/value pairs to set, using a dot-separated path into
  the nested JSON structure, e.g. `` `ModelParameters.f_wind` = 1.3 ``
  or `` `Simulation.Reference year` = 2020 ``. Values must be of the
  same type (numeric, logical, character) as the current value in the
  file.

- par_file:

  filepath; path to the `simstrat.par` file to edit. Defaults to
  `simstrat.par` in `path_simstrat`.

## Value

invisibly, the updated par list

## Examples

``` r
if (FALSE) { # \dontrun{
set_simstrat_param(path_simstrat, `ModelParameters.f_wind` = 1.3)
} # }
```
