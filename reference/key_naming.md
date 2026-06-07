# Model variable naming

A reference table for variable names between the models in AEME.

## Usage

``` r
key_naming
```

## Format

### `key_naming`

A data frame with 129 rows and 17 columns:

- var_aeme:

  AEME variable name

- dy_cd:

  DYRESM variable name

- glm_aed:

  GLM variable name

- gotm_wet:

  GOTM variable name

- gotm_fabm:

  GOTM-FABM variable name containing key-value names

- name_text:

  Regular text string variable name

- name_full:

  String variable name

- name_parse:

  Variable name for parsing

- units:

  Units of the variable. As defined using the
  [units](https://cran.r-project.org/package=units) package.

- conversion_aed:

  Unit conversion for GLM-AED

- default:

  Default value for the variable

- derived:

  Logical value if the variable is derived

- derived_from:

  Variable name that the variable is derived from

- min:

  Minimum value for the variable

- max:

  Maximum value for the variable

- keywords:

  Keywords associated with the variable

## Source

Package development.
