# Example dataset of parameters for the DYRESM-CAEDYM model

This dataset contains all parameters defined in the `.par` and `.cfg`
file. The values represent defaults from the standard configuration
file, with ±25% parameter ranges for use in sensitivity analysis or
model calibration.

## Usage

``` r
dy_cd_parameters
```

## Format

A data frame with 253 rows and 9 columns:

- model:

  Model to which the parameter belongs

- file:

  File in which the parameter is stored

- name:

  Name of the parameter

- value:

  Default value of the parameter

- min:

  Minimum range of the parameter

- max:

  Maximum range of the parameter

- group:

  Phytoplankton group for the parameter; only applies to phytoplankton
  parameters

- index:

  Index for parameters with multiple values in a vector (e.g.,
  `"sediment/sed_temp_mean"` in GLM-AED)

- module:

  Model module associated with the parameter, useful for identifying
  functional groupings

## Source

Created during package development.
