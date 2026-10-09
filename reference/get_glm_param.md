# Get one or more parameter values from a GLM-AED nml file

Companion to
[`set_glm_param()`](https://limnotrack.com/reference/set_glm_param.md)
for reading current values without needing an `aeme` object.

## Usage

``` r
get_glm_param(path_glm, name, glm_file = find_glm_nml(path_glm))
```

## Arguments

- path_glm:

  filepath; directory containing the GLM-AED configuration

- name:

  character vector; name(s) of the nml parameter(s) to read.

- glm_file:

  filepath; path to the nml file to edit. Defaults to the GLM
  hydrodynamic nml (`glm3.nml`/`glm4.nml`) found in `path_glm` via
  [`find_glm_nml()`](https://limnotrack.com/reference/find_glm_nml.md).
  Pass the `aed2.nml` path directly to edit AED parameters instead.

## Value

the parameter value if `name` has length 1, otherwise a named list of
values

## Examples

``` r
if (FALSE) { # \dontrun{
get_glm_param(path_glm, "Kw")
get_glm_param(path_glm, c("Kw", "coef_mix_hyp"))
} # }
```
