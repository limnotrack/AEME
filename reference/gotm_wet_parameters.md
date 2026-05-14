# Example dataset of parameters for the GOTM-WET model

All the parameters within the `gotm.yaml` and `fabm.yaml` file. This
includes three phytoplankton groups (greens, cyanobacteria an diatoms),
one zooplankton group (cladocerans). This has the values in the default
file and 25 % parameter ranges for sensitivity analysis or model
calibration.

## Usage

``` r
gotm_wet_parameters
```

## Format

A data frame with 628 rows and 14 columns:

- model:

  Model for the parameter

- file:

  File in which the parameter is stored

- name:

  Name of the parameter

- value:

  Value of the parameter

- min:

  Minimum range of the parameter

- max:

  Maximum range of the parameter

- group:

  Phytoplankton Group for the parameter, only applies to phytoplankton
  parameters

- index:

  Index for the parameter in the model file, only used for parameters
  that have multiple values in a vector such as "sediment/sed_temp_mean"
  in GLM-AED

- module:

  Module for the parameter in the model, useful to help identify
  parameters

- par:

  Short name for the parameter

- logical:

  Logical value if the parameter is boolean

- logical_val:

  Value if the parameter is boolean

- char:

  Logical value if the parameter is character

- char_val:

  Value if the parameter is character

## Source

Package development.
