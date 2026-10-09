# Get one or more parameter values from a GOTM-WET `gotm.yaml` file

Companion to
[`set_gotm_param()`](https://limnotrack.com/reference/set_gotm_param.md)
for reading current values without needing an `aeme` object.

## Usage

``` r
get_gotm_param(path_gotm, name, yaml_file = file.path(path_gotm, "gotm.yaml"))
```

## Arguments

- path_gotm:

  filepath; directory containing the GOTM-WET configuration

- name:

  character vector; dot-separated path(s) to read, e.g. `"time.dt"` or
  `c("time.dt", "location.latitude")`.

- yaml_file:

  filepath; path to the `gotm.yaml` file to edit. Defaults to
  `gotm.yaml` in `path_gotm`.

## Value

the parameter value if `name` has length 1, otherwise a named list of
values

## Examples

``` r
if (FALSE) { # \dontrun{
get_gotm_param(path_gotm, "time.dt")
get_gotm_param(path_gotm, c("time.dt", "location.latitude"))
} # }
```
