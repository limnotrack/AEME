# Set one or more parameter values in a GLM-AED nml file

Thin wrapper around
[`read_nml()`](https://limnotrack.com/reference/read_nml.md)/[`set_nml()`](https://limnotrack.com/reference/set_nml.md)/[`write_nml()`](https://limnotrack.com/reference/write_nml.md)
for editing a single GLM-AED `.nml` file in place, without needing an
`aeme` object. Intended for a GLM-AED-only workflow where a user just
wants to tweak parameters, run the model, and load the output.

## Usage

``` r
set_glm_param(path_glm, ..., glm_file = find_glm_nml(path_glm))
```

## Arguments

- path_glm:

  filepath; directory containing the GLM-AED configuration

- ...:

  named parameter/value pairs to set, e.g. `Kw = 0.5`,
  `coef_mix_hyp = 0.3`. Values must be of the same type (numeric,
  logical, character) as the current value in the nml file.

- glm_file:

  filepath; path to the nml file to edit. Defaults to the GLM
  hydrodynamic nml (`glm3.nml`/`glm4.nml`) found in `path_glm` via
  [`find_glm_nml()`](https://limnotrack.com/reference/find_glm_nml.md).
  Pass the `aed2.nml` path directly to edit AED parameters instead.

## Value

invisibly, the updated nml object

## Examples

``` r
if (FALSE) { # \dontrun{
set_glm_param(path_glm, Kw = 0.5, coef_mix_hyp = 0.3)
set_glm_param(path_glm, glm_file = file.path(path_glm, "aed", "aed2.nml"),
              p_max = 1.2)
} # }
```
