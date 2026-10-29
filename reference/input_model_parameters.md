# Input model parameters

Input model parameters

## Usage

``` r
input_model_parameters(aeme, model, param, path, derive_totals = TRUE)
```

## Arguments

- aeme:

  Aeme object.

- model:

  character vector; models to use. One or more of `"dy_cd"`,
  `"glm_aed"`, `"gotm_wet"`, `"simstrat_aed2"`. Defaults to all models
  if not found in `aeme`.

- param:

  data.frame; parameters to input into the model configuration files.
  Zone-ratio and zone-offset rows made by
  [`zone_ratio_param()`](https://limnotrack.com/AEME/reference/zone_ratio_param.md)
  and
  [`zone_offset_param()`](https://limnotrack.com/AEME/reference/zone_offset_param.md)
  are expanded to per-zone values first (see
  [`expand_zone_ratios()`](https://limnotrack.com/AEME/reference/expand_zone_ratios.md)).

- path:

  character; directory where input files are located. Defaults to the
  path stored in `aeme`, or the current working directory if not set.

- derive_totals:

  logical; if `TRUE` (default), the `aed_totals` block of the GLM-AED
  `aed.nml` is re-derived from the phytoplankton parameters when
  `aed.nml` or `aed_phyto_pars.csv` parameters are applied, overwriting
  any manual edits to that block. Set `FALSE` to leave `aed_totals` as
  it is.

## Value

Aeme object with parameters input into model configuration files
