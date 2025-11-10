# Example dataframe used for calibrating the biogeochemistry in the GOTM-WET model.

All the parameters within the fabm.yaml file. This includes three
phytoplankton groups (greens, cyanobacteria an diatoms), one zooplankton
group (cladocerans). This has the values in the default file and 25
ranges for sensitivity analysis.

## Usage

``` r
gotm_wet_parameters
```

## Format

\## \`gotm_wet_parameters\` A data frame with 182 rows and 7 columns:

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

- var:

  Maximum range of the parameter

## Source

Package development.
