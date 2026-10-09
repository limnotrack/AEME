# Get one or more parameter values from a Simstrat-AED2 `simstrat.par` file

Companion to
[`set_simstrat_param()`](https://limnotrack.com/reference/set_simstrat_param.md)
for reading current values without needing an `aeme` object.

## Usage

``` r
get_simstrat_param(
  path_simstrat,
  name,
  par_file = file.path(path_simstrat, "simstrat.par")
)
```

## Arguments

- path_simstrat:

  filepath; directory containing the Simstrat-AED2 configuration

- name:

  character vector; dot-separated path(s) to read, e.g.
  `"ModelParameters.f_wind"` or
  `c("Simulation.Reference year", "ModelParameters.lat")`.

- par_file:

  filepath; path to the `simstrat.par` file to edit. Defaults to
  `simstrat.par` in `path_simstrat`.

## Value

the parameter value if `name` has length 1, otherwise a named list of
values

## Examples

``` r
if (FALSE) { # \dontrun{
get_simstrat_param(path_simstrat, "ModelParameters.f_wind")
get_simstrat_param(path_simstrat, c("ModelParameters.f_wind", "ModelParameters.lat"))
} # }
```
