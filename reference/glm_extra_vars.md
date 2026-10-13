# GLM-AED sediment, flux and diagnostic output variables

A registry of the GLM-AED output variables that sit outside the regular
`key_naming` set – sediment temperature and heat flux, sediment-water
fluxes (lake-wide and by sediment zone) and sediment pools. They are
read by
[`read_glm_output()`](https://limnotrack.com/AEME/reference/read_glm_output.md)
straight from `output.nc` and stored under their raw netCDF name (in the
`diag`/`sediment` sub-lists of the model output, or flat for lake-wide
series). This table gives each an AEME name, label and units so they can
be requested by either name, e.g. with the `extra_vars` argument of
[`run_aeme()`](https://limnotrack.com/AEME/reference/run_aeme.md) or in
[`get_var()`](https://limnotrack.com/AEME/reference/get_var.md).

## Usage

``` r
glm_extra_vars
```

## Format

### `glm_extra_vars`

A data frame with 63 rows and 7 columns:

- var_aeme:

  AEME variable name

- glm_aed:

  Raw GLM-AED netCDF variable name

- group:

  Where the variable is stored in the model output: `"sediment"`
  (sub-list `sediment`), `"diag"` (sub-list `diag`) or `"core"` (flat,
  keyed by the raw name)

- dims:

  Dimensions of the variable in the netCDF file

- name_text:

  Regular text variable name

- units:

  Units, as written by GLM-AED

- keywords:

  Keywords for searching

## Source

Package development; generated from GLM-AED 4.x `output.nc`.

## Details

Naming: `SED_*` are sediment-zone variables (`nzones`, and `sed_layers`
for `SED_temp`); `SEDL_*` are the matching lake-wide `(time)` series
GLM-AED writes alongside them.
