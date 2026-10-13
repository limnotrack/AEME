# Input model parameters

Input model parameters

## Usage

``` r
input_model_parameters(aeme, model, param, path)
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

## Value

Aeme object with parameters input into model configuration files
