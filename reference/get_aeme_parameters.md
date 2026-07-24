# Make parameters dataframe for AEME

Make parameters dataframe for AEME

## Usage

``` r
get_aeme_parameters(model, file, module, name, par)
```

## Arguments

- model:

  character vector; models to use. One or more of `"dy_cd"`,
  `"glm_aed"`, `"gotm_wet"`, `"simstrat_aed2"`. Defaults to all models
  if not found in `aeme`.

- file:

  character; vector of file names to retrieve.

- module:

  character; vector of modules to retrieve parameters for.

- name:

  character; vector of parameter names to retrieve.

- par:

  character; vector of parameters to retrieve.

## Value

dataframe; of parameters filtered by model, module and parameter.
