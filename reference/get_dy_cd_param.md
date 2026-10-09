# Get one or more parameter values from a DYRESM-CAEDYM configuration

Companion to
[`set_dy_cd_param()`](https://limnotrack.com/reference/set_dy_cd_param.md)
for reading current values without an `aeme` object.

## Usage

``` r
get_dy_cd_param(
  path_dy,
  name,
  cfg_file = find_dy_cd_cfg(path_dy),
  par_file = file.path(path_dy, "dyresm3p1.par")
)
```

## Arguments

- path_dy:

  filepath; directory containing the DYRESM-CAEDYM configuration (the
  `dy_cd` model directory).

- name:

  character vector; name(s) of the parameter(s) to read – the same
  friendly names
  [`set_dy_cd_param()`](https://limnotrack.com/reference/set_dy_cd_param.md)
  accepts.

- cfg_file:

  filepath; the `<lakename>.cfg` file to edit. Defaults to the one found
  in `path_dy` via
  [`find_dy_cd_cfg()`](https://limnotrack.com/reference/find_dy_cd_cfg.md).

- par_file:

  filepath; the `dyresm3p1.par` file to edit. Defaults to
  `dyresm3p1.par` in `path_dy`.

## Value

the parameter value if `name` has length 1, otherwise a named list of
values. Numeric-looking values are returned as numbers, others (e.g.
`start_date`, `run_caedym`'s `.TRUE.`/`.FALSE.`) as strings.

## Examples

``` r
if (FALSE) { # \dontrun{
get_dy_cd_param(path_dy, "Kw")
get_dy_cd_param(path_dy, c("Kw", "max_layer_thickness", "eta_S"))
} # }
```
