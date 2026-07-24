# Example dataframe used for calibrating the Simstrat-AED2 model.

Physical (hydrodynamic) parameters from the Simstrat `.par` file and a
subset of biogeochemical initial-concentration parameters from the
`aed2.nml` file (oxygen, carbon, silica, nitrogen, phosphorus, and
organic matter modules). This has the values in the default template
files and 50 % parameter ranges for sensitivity analysis.

## Usage

``` r
simstrat_aed2_parameters
```

## Format

`simstrat_aed2_parameters` A data frame with columns:

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

- module:

  Whether the parameter belongs to the "hydrodynamic" or "bgc"
  (biogeochemical) part of the model

- par:

  Short parameter name

## Source

Package development.
