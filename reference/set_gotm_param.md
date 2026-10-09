# Set one or more parameter values in a GOTM-WET `gotm.yaml` file

Thin, `aeme`-free wrapper for editing a GOTM-WET `gotm.yaml` file in
place. Intended for a GOTM-WET-only workflow where a user just wants to
tweak parameters, run the model, and load the output.

## Usage

``` r
set_gotm_param(path_gotm, ..., yaml_file = file.path(path_gotm, "gotm.yaml"))
```

## Arguments

- path_gotm:

  filepath; directory containing the GOTM-WET configuration

- ...:

  named parameter/value pairs to set, using a dot-separated path into
  the nested yaml structure, e.g. `` `time.dt` = 1800 `` or
  `` `location.latitude` = -36.9 ``. Values must be of the same type
  (numeric, logical, character) as the current value in the file.

- yaml_file:

  filepath; path to the `gotm.yaml` file to edit. Defaults to
  `gotm.yaml` in `path_gotm`.

## Value

invisibly, the updated yaml list

## Examples

``` r
if (FALSE) { # \dontrun{
set_gotm_param(path_gotm, `time.dt` = 1800)
} # }
```
