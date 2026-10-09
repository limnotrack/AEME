# Remove parameter(s) from Aeme object

Remove parameter(s) from Aeme object

## Usage

``` r
remove_param(aeme, name, model, file)
```

## Arguments

- aeme:

  Aeme object.

- name:

  character vector with names of parameters to remove. If missing, all
  parameters (within `model`/`file`, if supplied) are removed.

- model:

  character vector; only remove parameters for these models. If missing,
  parameters are removed regardless of model.

- file:

  character vector; only remove parameters in these files. If missing,
  parameters are removed regardless of file.

## Value

Aeme object with parameters removed
