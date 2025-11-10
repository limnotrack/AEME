# Make parameters dataframe for AEME

Make parameters dataframe for AEME

## Usage

``` r
get_aeme_parameters(model, module, name, par)
```

## Arguments

- model:

  vector; of models to be used. Can be \`dy_cd\`, \`glm_aed\`,
  \`gotm_wet\`.

- module:

  character; vector of modules to retrieve parameters for.

- name:

  character; vector of parameter names to retrieve.

- par:

  character; vector of parameters to retrieve.

## Value

dataframe; of parameters filtered by model, module and parameter.
