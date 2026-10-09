# Set initial conditions for a GLM-AED simulation

Thin wrapper for editing the initial temperature/salinity profile and
water-quality initial values of a GLM-AED hydrodynamic nml file in
place, without needing an `aeme` object. Intended for a GLM-AED-only
workflow where a user just wants to tweak initial conditions, run the
model, and load the output.

## Usage

``` r
set_glm_init(
  path_glm,
  temp = NULL,
  salt = NULL,
  wq_init = NULL,
  glm_file = find_glm_nml(path_glm)
)
```

## Arguments

- path_glm:

  filepath; directory containing the GLM-AED configuration

- temp:

  numeric; new initial water temperature profile. Recycled to the number
  of depths in `init_profiles`. `NULL` (default) leaves it unchanged.

- salt:

  numeric; new initial salinity profile, same recycling rule as `temp`.
  `NULL` (default) leaves it unchanged.

- wq_init:

  named list; new initial values for water quality variables, e.g.
  `list(NIT_amm = 0.5, CHM_oxy = 300)`. Names must match
  `init_profiles$wq_names` in the nml file. Each value is recycled
  across depths. `NULL` (default) leaves water quality initial values
  unchanged.

- glm_file:

  filepath; path to the nml file to edit. Defaults to the GLM
  hydrodynamic nml (`glm3.nml`/`glm4.nml`) found in `path_glm` via
  [`find_glm_nml()`](https://limnotrack.com/reference/find_glm_nml.md).

## Value

invisibly, the updated nml object

## Details

Existing profile depths (`init_profiles$the_depths`) are left unchanged
– `temp`/`salt`/`wq_init` values are recycled (via
[`rep_len()`](https://rdrr.io/r/base/rep.html)) across however many
depths are already defined.

## Examples

``` r
if (FALSE) { # \dontrun{
set_glm_init(path_glm, temp = seq(20, 10, length.out = 10))
set_glm_init(path_glm, wq_init = list(NIT_amm = 0.5, CHM_oxy = 300))
} # }
```
