# Example dataframe used for calibrating the biogeochemistry in the AEME models.

An example dataframe used for inputting and calibrating AEME models.

## Usage

``` r
aeme_parameters_bgc
```

## Format

\## \`aeme_parameters_bgc\` A data frame with 30 rows and 7 columns:

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

## Source

Package development.
